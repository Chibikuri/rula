// This is entory point to generate code from AST
use super::error::*;
use super::IResult;

use crate::network::qnic_wrapper::QnicInterfaceWrapper;
use crate::rulep::action::v2::ActionClauses;
use crate::rulep::action::Action;
use crate::rulep::condition::v1::*;
use crate::rulep::condition::Condition;
use crate::rulep::ruleset::{Rule, RuleSet};
use rula_parser::parser::ast::*;

use once_cell::sync::OnceCell;
use proc_macro2::{Span, TokenStream};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::format;
use std::rc::Rc;
use std::sync::Mutex;
use syn::LitFloat;

// These are used but only in quote! macro
#[allow(unused_imports)]
use mock_components::hardware::result::MeasResult;
#[allow(unused_imports)]
use std::net::SocketAddr;

// Right now, compilation is all single thread. In the future, this could be multi thread.
// Mutex would be a good choice for that.
type MutexRuleSet = Mutex<RuleSet<ActionClauses>>;
static RULESET: OnceCell<MutexRuleSet> = OnceCell::new();

type MutexRuleHashMap = Mutex<HashMap<String, Rule<ActionClauses>>>;
static RULE_TABLE: OnceCell<MutexRuleHashMap> = OnceCell::new();

static QNIC_INTERFACE_TABLE: OnceCell<Mutex<HashMap<String, QnicInterfaceWrapper>>> =
    OnceCell::new();

/// Generate corresponding rust code from ast
/// Every nested generators returns a piece of TokenStream
pub fn generate(
    ast_tree: Vec<AstNode>,
    with_ruleset: bool,
) -> IResult<(TokenStream, Option<RuleSet<ActionClauses>>)> {
    // initialize all the global values (RULESET, RULE_TABLE)
    initialize_singleton();

    // RuLa starts here
    let mut rula_program = quote!();
    for ast_node in ast_tree {
        match ast_node {
            AstNode::RuLa(rula) => rula_program = generate_rula(&rula).unwrap(),
            AstNode::PlaceHolder => {
                return Err(RuLaCompileError::RuLaInitializationError(
                    InitializationError::new("at very first generation"),
                ))
            }
        }
    }
    // All ast are supposed to be evaluated here
    let rula_token_stream = quote!(
        mod rula{
            #rula_program
        }
        pub fn main(){

        }
    );
    if with_ruleset {
        let opt_ruleset = RULESET.get();
        match opt_ruleset {
            Some(ruleset) => Ok((rula_token_stream, Some(ruleset.lock().unwrap().clone()))),
            None => panic!("failed to generate RuleSet"),
        }
    } else {
        Ok((rula_token_stream, None))
    }
}

fn initialize_singleton() {
    // Set the initial RuleSet so that other RuleSet call can only get without init
    assert!(RULESET.get().is_none());
    assert!(RULE_TABLE.get().is_none());
    assert!(QNIC_INTERFACE_TABLE.get().is_none());

    // closure to initialize with empty ruleset
    let empty_ruleset = || Mutex::new(RuleSet::<ActionClauses>::new("empty_ruleset"));
    let _ = RULESET.get_or_init(empty_ruleset);

    // Rule tables to store rule instances
    let initialize_rule_table = || Mutex::new(HashMap::<String, Rule<ActionClauses>>::new());
    let _ = RULE_TABLE.get_or_init(initialize_rule_table);

    // Interface wrapper table that is an endpoint of qnic function call
    let initialize_interface_table = || Mutex::new(HashMap::<String, QnicInterfaceWrapper>::new());
    let _ = QNIC_INTERFACE_TABLE.get_or_init(initialize_interface_table);
}

fn generate_rula(rula: &RuLa) -> IResult<TokenStream> {
    match &*rula.rula {
        RuLaKind::Program(program) => {
            return Ok(generate_program(&program).unwrap());
        }
        RuLaKind::Ignore => return Ok(quote!()),
        RuLaKind::Eoi => {
            return Ok(quote!(
                #[doc = "End of input"]
            ))
        }
        RuLaKind::PlaceHolder => {
            return Err(RuLaCompileError::RuLaInitializationError(
                InitializationError::new("at generate rula"),
            ))
        }
    }
}

fn generate_program(program: &Program) -> IResult<TokenStream> {
    let mut stmts = vec![];
    for program_block in &program.programs {
        match program_block {
            ProgramKind::Stmt(stmt) => stmts.push(generate_stmt(&stmt, None).unwrap()),
        }
    }
    Ok(quote!(#(#stmts )*))
}

fn generate_interface(interface: &Interface) -> IResult<TokenStream> {
    let mut interface_names = vec![];
    let interface_table = QNIC_INTERFACE_TABLE
        .get()
        .expect("Failed to get qnic interface table");
    for i in &interface.interface {
        interface_table
            .lock()
            .unwrap()
            .insert(*i.name.clone(), QnicInterfaceWrapper::place_holder());
        interface_names.push(generate_ident(i).unwrap());
    }
    let interface_group_name = match &*interface.group_name {
        Some(group_name) => generate_ident(&group_name).unwrap(),
        None => quote!(INTERFACE),
    };

    Ok(quote!(
        use once_cell::sync::OnceCell;
        use std::sync::Mutex;
        use std::collections::HashMap;
        use std::net::IpAddr;
        use mock_components::hardware::qnic::*;

        static #interface_group_name: OnceCell<Mutex<InterfaceGroup>> = OnceCell::new();

        #[derive(Debug)]
        pub struct InterfaceGroup{
            pub interfaces: HashMap<String, QNicInterface>,
        }

        impl InterfaceGroup{
            pub fn new() -> Self{
                InterfaceGroup{interfaces: HashMap::new()}
            }

            pub fn add_interface(&mut self, name: &str, interface: QNicInterface){
                self.interfaces.insert(name.to_string(), interface);
            }
        }

        #[derive(Debug)]
        pub struct QNicInterface{
            pub message_box: HashMap<RuleIdentifier, Message>,
            pub qnic: MockQnic,
        }

        impl QNicInterface{
            pub fn new() -> QNicInterface{
                QNicInterface{
                    message_box: HashMap::new(),
                    qubits: HashMap::new()
                }
            }
            pub fn request_resource() -> QubitInterface{
                /// 0. Look up qubit states
                QubitInterface{qubit_address:10}
            }
        }
        #[derive(Debug)]
        pub struct Message{
            pub socket_addr: SocketAddr,
            pub meas_result: MeasResult,
        }

        #[derive(Debug)]
        pub struct RuleIdentifier{
            pub qnic_address: IpAddr,
            pub rule_id: u32,
        }

        #[derive(Debug)]
        pub struct QubitInterface{
            pub qubit_address: u64,
        }

        pub fn interface_init(){
            let interface = #interface_group_name.get_or_init(|| Mutex::new(InterfaceGroup::new()));
            for inter in vec![#(#interface_names),*]{
                interface.lock().unwrap().add_interface(inter, QNicInterface::new());
            }
        }
    ))
}

fn generate_stmt(stmt: &Stmt, rule: Option<&String>) -> IResult<TokenStream> {
    let generated_stmt = match &*stmt.kind {
        StmtKind::Let(let_stmt) => Ok(generate_let(let_stmt, rule, false).unwrap()),
        StmtKind::Interface(interface) => Ok(generate_interface(&interface).unwrap()),
        StmtKind::Expr(expr) => Ok(generate_expr(&expr, rule).unwrap()),
        StmtKind::PlaceHolder => Err(RuLaCompileError::RuLaInitializationError(
            InitializationError::new("at generate rula"),
        )),
    }
    .unwrap();
    Ok(quote!(#generated_stmt))
}

/// Generate 'let' statement
///
/// # Arguments:
///
/// * 'let_stmt' - (Let) Ast node for Let statement
/// * 'rule' - (Rule) Static Rule definition
/// * 'in_watch' - (bool) boolean value if this let statement is in watch or not.
///                 this can be included AST Node later
///
fn generate_let(let_stmt: &Let, rule: Option<&String>, in_watch: bool) -> IResult<TokenStream> {
    if &*let_stmt.ident == &Ident::place_holder() {
        return Err(RuLaCompileError::FailedToSetValueError);
    }
    let identifier = generate_ident(&*let_stmt.ident).unwrap();
    let expr = generate_expr(&*let_stmt.expr, rule).unwrap();
    Ok(quote!(
        let mut #identifier = #expr;
    ))
}

fn generate_expr(expr: &Expr, rule: Option<&String>) -> IResult<TokenStream> {
    match &*expr.kind {
        ExprKind::Import(import_expr) => Ok(generate_import(&import_expr).unwrap()),
        ExprKind::If(if_expr) => Ok(generate_if(&if_expr).unwrap()),
        ExprKind::For(for_expr) => Ok(generate_for(&for_expr).unwrap()),
        ExprKind::While(while_expr) => Ok(generate_while(&while_expr).unwrap()),
        ExprKind::FnDef(fn_def_expr) => Ok(generate_fn_def(&fn_def_expr).unwrap()),
        ExprKind::FnCall(fn_call_expr) => Ok(generate_fn_call(&fn_call_expr, rule).unwrap()),
        ExprKind::Struct(struct_expr) => Ok(generate_struct(&struct_expr).unwrap()),
        ExprKind::Return(return_expr) => Ok(generate_return(&return_expr).unwrap()),
        ExprKind::Match(match_expr) => Ok(generate_match(&match_expr).unwrap()),
        ExprKind::Comp(comp_expr) => Ok(generate_comp(&comp_expr).unwrap()),
        ExprKind::RuleSetExpr(ruleset_expr) => Ok(generate_ruleset_expr(&ruleset_expr).unwrap()),
        ExprKind::RuleExpr(rule_expr) => Ok(generate_rule(&rule_expr).unwrap()),
        ExprKind::CondExpr(cond_expr) => Ok(generate_cond(&cond_expr, rule).unwrap()),
        ExprKind::ActExpr(act_expr) => Ok(generate_act(&act_expr, rule).unwrap()),
        ExprKind::VariableCallExpr(variable_call_expr) => {
            Ok(generate_variable_call(&variable_call_expr, rule).unwrap())
        }
        ExprKind::Array(array_expr) => Ok(generate_array(&array_expr).unwrap()),
        ExprKind::Lit(lit_expr) => Ok(generate_lit(&lit_expr).unwrap()),
        ExprKind::Term(term_expr) => Ok(generate_term(*term_expr).unwrap()),
        ExprKind::PlaceHolder => Err(RuLaCompileError::RuLaInitializationError(
            InitializationError::new("at generating expression"),
        )),
    }
}

fn generate_import(import_expr: &Import) -> IResult<TokenStream> {
    // Convert a set of paths into a set of identifiers
    let path = import_expr.path.convert_to_ident();
    // Vector to store all paths
    // e.g. [hello::world, good::evening::Yall]
    let mut paths = vec![];
    for path_ident in path.iter() {
        let mut single_path = vec![];
        let quoted_path = if path_ident.name.contains("/") {
            let splitted = path_ident.name.split("/");
            // This is not clean
            for sp in splitted.into_iter() {
                let new_ident = Ident::new(sp, None);
                let path_fragment = generate_ident(&new_ident).unwrap();
                single_path.push(path_fragment)
            }
            let path_head = &single_path[0];
            let path_left = &single_path[1..];
            quote!(
                #path_head #(::#path_left)*
            )
        } else {
            let path_ident = generate_ident(path_ident).unwrap();
            quote!(#path_ident)
        };
        paths.push(quoted_path);
    }
    Ok(quote!(
        #(use #paths; )*
    ))
}

fn generate_if(if_expr: &If) -> IResult<TokenStream> {
    // block could have invalid expression here.
    let block_quote = generate_expr(&*if_expr.block, None).unwrap();
    let stmt_quote = generate_stmt(&*if_expr.stmt, None).unwrap();
    if if_expr.elif.len() > 0 {
        // no elif statement
        if if_expr.elif[0] == None {
            match &*if_expr.els {
                // With else statement
                Some(els_stmt) => {
                    let els_stmt_quote = generate_stmt(&els_stmt, None).unwrap();
                    return Ok(quote!(
                        if #block_quote{
                            #stmt_quote
                        }else{
                            #els_stmt_quote
                        }
                    ));
                }
                None => {
                    // No error statement
                    // This could be error in rust code
                    return Ok(quote!(
                        if #block_quote{
                            #stmt_quote
                        }
                    ));
                }
            }
        } else {
            // No elif expr
            let mut elif_quotes = vec![];
            for elif_expr in &*if_expr.elif {
                match elif_expr {
                    Some(if_expr) => elif_quotes.push(generate_if(&if_expr).unwrap()),
                    None => {
                        unreachable!()
                    }
                }
            }

            match &*if_expr.els {
                Some(els_stmt) => {
                    let els_stmt_quote = generate_stmt(&els_stmt, None).unwrap();
                    Ok(quote!(
                        if #block_quote {
                            #stmt_quote
                        }#(else #elif_quotes)*

                        else{
                            #els_stmt_quote
                        }
                    ))
                }
                None => Ok(quote!(
                    if #block_quote {
                        #stmt_quote
                    }#(elif #elif_quotes)*
                )),
            }
        }
    } else {
        // Duplication?
        panic!("This should not happen in the current initialization");
    }
}

fn generate_for(for_expr: &For) -> IResult<TokenStream> {
    let mut ident_list = vec![];
    for ident in for_expr.pattern.iter() {
        ident_list.push(generate_ident(ident).unwrap());
    }
    let generator = generate_expr(&for_expr.generator, None).unwrap();
    let stmt = generate_stmt(&for_expr.stmt, None).unwrap();
    if ident_list.len() == 1 {
        let var = &ident_list[0];
        Ok(quote!(
            for #var in #generator {
                #stmt
            }
        ))
    } else if ident_list.len() > 1 {
        Ok(quote!(
            for (#(#ident_list),* ) in generator {
                #stmt
            }
        ))
    } else {
        Err(RuLaCompileError::RuLaInitializationError(
            InitializationError::new("at generating for expression"),
        ))
    }
}

fn generate_while(while_expr: &While) -> IResult<TokenStream> {
    let block = generate_expr(&while_expr.block, None).unwrap();
    let stmt = generate_stmt(&while_expr.stmt, None).unwrap();
    Ok(quote!(
        while #block{
            #stmt
        }
    ))
}

fn generate_fn_def(fn_def_expr: &FnDef) -> IResult<TokenStream> {
    let mut arguments = vec![];
    for ident in fn_def_expr.arguments.iter() {
        arguments.push(generate_ident(ident).unwrap());
    }
    let stmt = generate_stmt(&fn_def_expr.stmt, None).unwrap();
    // Here could have generics in the future
    Ok(quote!(
        pub fn ( #(#arguments),* ) {
            #stmt
        }
    ))
}
fn generate_fn_call(fn_call_expr: &FnCall, rule: Option<&String>) -> IResult<TokenStream> {
    let fn_name = generate_ident(&fn_call_expr.func_name).unwrap();
    match rule {
        Some(rule_content) => {}
        None => {}
    }
    Ok(quote!(
        #fn_name ()
    ))
}
fn generate_struct(struct_expr: &Struct) -> IResult<TokenStream> {
    let struct_name = generate_ident(&struct_expr.name).unwrap();
    let mut struct_items = vec![];
    for ident in struct_expr.items.iter() {
        struct_items.push(generate_ident(ident).unwrap());
    }
    Ok(quote!(
        struct #struct_name{
            #(#struct_items),*
        }
    ))
}
fn generate_return(return_expr: &Return) -> IResult<TokenStream> {
    let expr = generate_expr(&return_expr.target, None).unwrap();
    Ok(quote!(
        return #expr;
    ))
}

fn generate_match(match_expr: &Match) -> IResult<TokenStream> {
    Ok(quote!())
}

fn generate_comp(comp_expr: &Comp) -> IResult<TokenStream> {
    let lhs = generate_expr(&comp_expr.lhs, None).unwrap();
    let rhs = generate_expr(&comp_expr.rhs, None).unwrap();
    let op = match *comp_expr.comp_op {
        CompOpKind::Lt => quote!(<),
        CompOpKind::Gt => quote!(>),
        CompOpKind::LtE => quote!(<=),
        CompOpKind::GtE => quote!(>=),
        CompOpKind::Eq => quote!(==),
        CompOpKind::Nq => quote!(!=),
        CompOpKind::PlaceHolder => {
            return Err(RuLaCompileError::RuLaInitializationError(
                InitializationError::new("at generating comp expression"),
            ))
        }
    };
    Ok(quote!(#lhs #op #rhs))
}

fn generate_ruleset_expr(ruleset_expr: &RuleSetExpr) -> IResult<TokenStream> {
    // Generate RuleSet
    // generate portable format with rule information
    let ruleset_name = &*ruleset_expr.name.name;

    // Need to evaluate configuration here to generate multiple ruleset
    let config = &*ruleset_expr.config;

    // Static RuleSet that can be output of this compiler
    let glob_ruleset = RULESET.get().expect("Failed to get ruleset");
    glob_ruleset.lock().unwrap().update_name(ruleset_name);

    // Rules must be defined beforehand in this table
    let rule_table = RULE_TABLE.get().expect("Failed to get rule table");

    // Closure that gets rule_name and evaluate if the rule is inside the table or not
    // If that Rule is in the table, add it to RuleSet
    let rule_add_evaluater = |rule_name: &String| {
        if !rule_table.lock().unwrap().contains_key(rule_name) {
            return Err(RuLaCompileError::NoRuleFoundError);
        } else {
            let corr_rule = rule_table.lock().unwrap().get(rule_name).unwrap().clone();
            glob_ruleset.lock().unwrap().add_rule(corr_rule);
            Ok(())
        }
    };

    for rule in &ruleset_expr.rules {
        match rule {
            RuleIdentifier::FnCall(fn_call_expr) => {
                // Rule without any return values
                rule_add_evaluater(&*fn_call_expr.func_name.name).unwrap();
            }
            RuleIdentifier::Let(let_stmt) => {
                // Rule with return value
                // The name of the function must be in RULE_TABLE
                let expr = &*let_stmt.expr;
                match &*expr.kind {
                    ExprKind::FnCall(fn_call) => {
                        rule_add_evaluater(&*fn_call.func_name.name).unwrap();
                    }
                    _ => unreachable!("So far RuleIdentifier can only be FnCall"),
                }
            }
        }
    }
    Ok(quote!())
}

fn generate_rule(rule_expr: &RuleExpr) -> IResult<TokenStream> {
    // Prepare RULE_TABLE reference for later use
    let rule_table = RULE_TABLE.get().expect("Failed to get rule table");
    let interface_table = QNIC_INTERFACE_TABLE
        .get()
        .expect("Failed to get interface table");

    // Get the basis information of Rule
    let rule_name = &*rule_expr.name;

    // Check if there is a Rule with the same name
    if rule_table.lock().unwrap().contains_key(&*rule_name.name) {
        return Err(RuLaCompileError::RuleDuplicationError);
    }

    // Prepare an empty Rule
    let mut rule = Rule::<ActionClauses>::new(&rule_name.name);
    // When the ruleset is finalized, this interface name is replaced by actual interface name
    let rule_interfaces = &rule_expr.interface;
    // Setup interface placeholder
    for interface in rule_interfaces {
        // Interface wrapper
        if !interface_table
            .lock()
            .unwrap()
            .contains_key(&*interface.name)
        {
            // If the interface is not properly defined in the file,
            // this returns no interface found error
            return Err(RuLaCompileError::NoInterfaceFoundError);
        } else {
            match interface_table.lock().unwrap().get(&*interface.name) {
                // Providing ref would be better
                Some(target_interface) => {
                    // This can be just a reference to the interface
                    rule.add_interface(&interface.name, target_interface.clone());
                }
                None => return Err(RuLaCompileError::UnknownError),
            }
        }
    }

    // Set empty condition and action to be updated in the different functions
    let empty_condition = Condition::new(None);
    let empty_action = Action::<ActionClauses>::new(None);
    rule.set_condition(empty_condition);
    rule.set_action(empty_action);

    let rule_name_string = String::from(&*rule_name.name);
    // Shouldn't be better way?
    rule_table
        .lock()
        .unwrap()
        .insert(rule_name_string.clone(), rule);
    // generate the rule content expression with this
    let generated =
        generate_rule_content(&rule_expr.rule_content, Some(&rule_name_string)).unwrap();

    // RuLa runtime generation
    let rule_name_token = generate_ident(rule_name).unwrap();

    Ok(quote!(
        struct #rule_name_token{
        }
    ))
}

fn generate_rule_content(
    rule_content_expr: &RuleContentExpr,
    rule: Option<&String>,
) -> IResult<TokenStream> {
    // monitor expression is only used for RuLa runtime with interaction with RE
    let watch_expr = &*rule_content_expr.monitor_expr;
    let condition_expr = &*rule_content_expr.condition_expr;
    let action_expr = &rule_content_expr.action_expr;
    let (generated_watch, generated_cond, generated_act) = match rule {
        Some(rule_contents) => {
            let (w, c, a) = match watch_expr {
                Some(watcher) => (
                    generate_watch(watcher, Some(&rule_contents)).unwrap(),
                    generate_cond(condition_expr, Some(&rule_contents)).unwrap(),
                    generate_act(action_expr, Some(&rule_contents)).unwrap(),
                ),
                None => (
                    quote!(),
                    generate_cond(condition_expr, Some(&rule_contents)).unwrap(),
                    generate_act(action_expr, Some(&rule_contents)).unwrap(),
                ),
            };
            (w, c, a)
        }
        None => (quote!(), quote!(), quote!()),
    };
    let post_process = &rule_content_expr.post_processing;
    Ok(quote!())
}

fn generate_watch(watch_expr: &WatchExpr, rule_name: Option<&String>) -> IResult<TokenStream> {
    // watch clauses should be the same as conditions not yet be met
    // Get the rule instance in RULE_TABLE and add condition
    let rule_table = RULE_TABLE.get().expect("Failed to get Rule table");

    match rule_name {
        Some(name) => {
            for value in &watch_expr.watched_values {
                generate_let(value, Some(&name), true).unwrap();
                // watch can be
            }
            let condition_clause =
                ConditionClauses::EnoughResource(EnoughResource::new(1, None, None));
            rule_table
                .lock()
                .unwrap()
                .get_mut(name)
                .unwrap_or_else(|| panic!("Failed to get Rule {}", name))
                .condition
                .add_condition_clause(condition_clause);
        }
        None => todo!(),
    };
    Ok(quote!())
}

fn generate_cond(cond_expr: &CondExpr, rule_name: Option<&String>) -> IResult<TokenStream> {
    let mut condition = Condition::new(None);
    let generated_clauses = match rule_name {
        Some(name) => {
            for clause in &cond_expr.clauses {
                generate_awaitable(&clause, Some(name)).unwrap();
            }
        }
        None => {
            for clause in &cond_expr.clauses {
                generate_awaitable(&clause, None).unwrap();
            }
        }
    };
    Ok(quote!())
}

fn generate_awaitable(
    awaitable_expr: &Awaitable,
    rule_name: Option<&String>,
) -> IResult<TokenStream> {
    // these must be condition clauses
    let rule_table = RULE_TABLE.get().expect("Failed to get Rule Table");
    match rule_name {
        Some(name) => {
            match awaitable_expr {
                Awaitable::FnCall(fn_call) => {
                    // This should be flex
                    todo!("builtin functions used for conditions should be here")
                }
                Awaitable::VariableCallExpr(val_call) => {
                    // This should also be flexible
                    match val_call.variables.last() {
                        Some(val) => {
                            match val {
                                Callable::FnCall(fn_call) => {
                                    if *fn_call.func_name.name == "ready" {
                                        // EnoughResource can be done in watch expression
                                    }
                                }
                                _ => todo!(),
                            }
                        }
                        None => {
                            todo!()
                        }
                    }
                }
                Awaitable::Comp(comp) => {}
            }
        }
        None => {}
    }
    Ok(quote!())
}

fn generate_act(act_expr: &ActExpr, rule: Option<&String>) -> IResult<TokenStream> {
    Ok(quote!())
}

fn generate_variable_call(
    variable_call_expr: &VariableCallExpr,
    rule_name: Option<&String>,
) -> IResult<TokenStream> {
    let interface_table = QNIC_INTERFACE_TABLE
        .get()
        .expect("Failed to get interface table");

    // Singleton Rule ref here
    let rule_table = RULE_TABLE.get().expect("Failed to get rule table");

    let mut val_calls = vec![];
    // if the first variable is interface name, that supposed to be interface function call
    let mut eval_first_two = |rule_name: Option<&String>| {
        match &variable_call_expr.variables[0] {
            Callable::FnCall(fn_call) => {
                val_calls.push(generate_fn_call(fn_call, rule_name).unwrap())
            }
            Callable::Ident(identifier) => {
                if interface_table
                    .lock()
                    .unwrap()
                    .contains_key(&*identifier.name)
                {
                    // This is an interface definition with this name
                    // get function name for this interface
                    // FIXME: All the information call be access by function for now.
                    // This is supposed to be a builtin function of qnic interface
                    match &variable_call_expr.variables[1] {
                        Callable::FnCall(builtin_qnic_fn) => {
                            match builtin_qnic_fn.func_name.name.as_str() {
                                // Todo: Generate list of builtin function here
                                "request_resource" => {
                                    let enough_resource = interface_table
                                        .lock()
                                        .unwrap()
                                        .get(&*identifier.name)
                                        .unwrap()
                                        .request_resource(true)
                                        .unwrap();
                                    // Todo: refactoring
                                    match rule_name {
                                        Some(name) => {
                                            rule_table
                                                .lock()
                                                .unwrap()
                                                .get_mut(name)
                                                .unwrap_or_else(|| {
                                                    panic!("Unable to find the Rule {}", name)
                                                })
                                                .condition
                                                .add_condition_clause(enough_resource);
                                        }
                                        None => {
                                            todo!()
                                        }
                                    }
                                }
                                _ => todo!(),
                            }
                        }
                        _ => unreachable!("Qnic interface can only take functions"),
                    }
                } else {
                    // If this is not a qnic interface, just treat as usual identifier
                    val_calls.push(generate_ident(identifier).unwrap());
                    match &variable_call_expr.variables[1] {
                        Callable::FnCall(fn_call_expr) => {}
                        Callable::Ident(ident_expr) => {}
                    }
                }
            }
        }
    };

    match rule_name {
        Some(name) => {
            eval_first_two(Some(name));
            // FIXME: should combine these
            if variable_call_expr.variables.len() > 2 {
                for val in &variable_call_expr.variables[2..] {
                    match val {
                        Callable::FnCall(fn_call) => {
                            val_calls.push(generate_fn_call(fn_call, Some(name)).unwrap())
                        }
                        Callable::Ident(ident) => {
                            val_calls.push(generate_ident(ident).unwrap());
                        }
                    }
                }
            }
        }
        None => {
            eval_first_two(None);
            if variable_call_expr.variables.len() > 2 {
                for val in &variable_call_expr.variables[2..] {
                    match val {
                        Callable::FnCall(fn_call) => {
                            val_calls.push(generate_fn_call(fn_call, None).unwrap());
                        }
                        Callable::Ident(ident) => {
                            val_calls.push(generate_ident(ident).unwrap());
                        }
                    }
                }
            }
        }
    }
    Ok(quote!(
        #(#val_calls).*
    ))
}

fn generate_array(array_expr: &Array) -> IResult<TokenStream> {
    let mut items = vec![];
    for lits in array_expr.items.iter() {
        items.push(generate_lit(lits).unwrap());
    }
    Ok(quote!(vec![#(#items),*]))
}

fn generate_lit(lit: &Lit) -> IResult<TokenStream> {
    match &*lit.kind {
        LitKind::Ident(ident_lit) => Ok(generate_ident(ident_lit).unwrap()),
        LitKind::NumberLit(number_lit) => {
            let val = LitFloat::new(&*number_lit.value.name, Span::call_site());
            Ok(quote!(#val))
        }
        _ => todo!(),
    }
}

fn generate_term(term_expr: f64) -> IResult<TokenStream> {
    // We could make Term struct instead of direct calc
    // For now, this function just returns calc result as f64
    let val = LitFloat::new(&term_expr.to_string(), Span::call_site());
    Ok(quote!(#val))
}
// Generate identifier token from Ident ast
fn generate_ident(ident: &Ident) -> IResult<TokenStream> {
    let identifier = format_ident!("{}", *ident.name);
    match &*ident.type_hint {
        Some(hint) => {
            let type_hint = generate_type_hint(hint).unwrap();
            Ok(quote!(#identifier:#type_hint))
        }
        None => Ok(quote!(#identifier)),
    }
}

fn generate_type_hint(type_hint: &TypeDef) -> IResult<TokenStream> {
    match type_hint {
        TypeDef::Boolean => Ok(quote!(bool)),
        TypeDef::Integer32 => Ok(quote!(i32)),
        TypeDef::Integer64 => Ok(quote!(i64)),
        TypeDef::Str => Ok(quote!(String)),
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use rula_derive::trace;

    use super::*;

    // ident tests
    #[test]
    fn test_ident_no_type_hint() {
        let test_ident = Ident::new("hello", None);
        let test_stream = generate_ident(&test_ident).unwrap();
        assert_eq!("hello", test_stream.to_string());
    }
    #[test]
    fn test_ident_with_type_hint() {
        let test_ident = Ident::new("hello", Some(TypeDef::Boolean));
        let test_stream = generate_ident(&test_ident).unwrap();
        assert_eq!("hello : bool", test_stream.to_string());
    }

    // Import test
    #[test]
    fn test_simple_import() {
        // import hello;
        let expected_paths = vec![["hello"].iter().collect()];
        let test_import = Import::new(PathKind::from(expected_paths));
        let test_stream = generate_import(&test_import).unwrap();
        assert_eq!("use hello ;", test_stream.to_string());
    }

    // If test
    #[test]
    fn test_simple_if() {
        // if (block) {expression}
        let simple_if = If::new(
            // (block)
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "block", None,
            ))))),
            // {expression}
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("expression", None)),
            ))))),
            // elif ~
            None,
            // else ~
            None,
        );
        let test_stream = generate_if(&simple_if).unwrap();
        assert_eq!("if block { expression }", test_stream.to_string());
    }

    #[test]
    fn test_if_else() {
        // if (block) {expression} else {expression2}
        let if_else = If::new(
            // (block)
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "block", None,
            ))))),
            // {expression}
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("expression", None)),
            ))))),
            // elif ~
            None,
            // else ~
            Some(Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
                Lit::new(LitKind::Ident(Ident::new("expression2", None))),
            ))))),
        );
        let test_stream = generate_if(&if_else).unwrap();
        assert_eq!(
            "if block { expression } else { expression2 }",
            test_stream.to_string()
        );
    }

    #[test]
    fn test_if_elif_else() {
        // if(block){expression} else if (block2){expression2} else {expression3}
        let if_elif_else = If::new(
            // (block)
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "block", None,
            ))))),
            // {expression}
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("expression", None)),
            ))))),
            // elif ~
            Some(If::new(
                // else if (block)
                Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                    "block2", None,
                ))))),
                // else if () {statement2;};
                Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                    LitKind::Ident(Ident::new("expression2", None)),
                ))))),
                None,
                None,
            )),
            // else ~
            Some(Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
                Lit::new(LitKind::Ident(Ident::new("expression3", None))),
            ))))),
        );
        let test_stream = generate_if(&if_elif_else).unwrap();
        assert_eq!(
            "if block { expression } else if block2 { expression2 } else { expression3 }",
            test_stream.to_string()
        );
    }

    // for test
    #[test]
    fn test_simple_for_generation() {
        // for (i) in generator {hello}
        let simple_for = For::new(
            vec![Ident::new("i", None)],
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "generator",
                None,
            ))))),
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("hello", None)),
            ))))),
        );
        let test_stream = generate_for(&simple_for).unwrap();
        assert_eq!("for i in generator { hello }", test_stream.to_string());
    }

    #[test]
    fn test_multi_arg_for_generation() {
        // for (a, b, c) in generator{hello}
        let multi_for = For::new(
            vec![
                Ident::new("a", None),
                Ident::new("b", None),
                Ident::new("c", None),
            ],
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "generator",
                None,
            ))))),
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("hello", None)),
            ))))),
        );
        let test_stream = generate_for(&multi_for).unwrap();
        assert_eq!(
            "for (a , b , c) in generator { hello }",
            test_stream.to_string()
        );
    }
    // While test
    #[test]
    fn test_simple_while() {
        let simple_while = While::new(
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "count", None,
            ))))),
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("expression", None)),
            ))))),
        );
        let test_stream = generate_while(&simple_while).unwrap();
        assert_eq!("while count { expression }", test_stream.to_string());
    }

    // FnDef test
    #[test]
    fn test_simple_fn_def() {
        // fn(block:i32, hello:str){expression}
        let simple_fn_def = FnDef::new(
            vec![
                Ident::new("block", Some(TypeDef::Integer32)),
                Ident::new("hello", Some(TypeDef::Str)),
            ],
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("expression", None)),
            ))))),
        );
        let test_stream = generate_fn_def(&simple_fn_def).unwrap();
        assert_eq!(
            "pub fn (block : i32 , hello : String) { expression }",
            test_stream.to_string()
        );
    }

    // FnCall test
    #[test]
    fn test_simple_fn_call() {
        // range()
        let simple_fn_call = FnCall::new(Ident::new("range", None), vec![]);
        let test_stream = generate_fn_call(&simple_fn_call, None).unwrap();
        assert_eq!("range ()", test_stream.to_string());
    }

    // Struct test
    #[test]
    fn test_simple_struct() {
        // struct Test{flag: bool}
        let simple_struct = Struct::new(
            Ident::new("Test", None),
            vec![Ident::new("flag", Some(TypeDef::Boolean))],
        );
        let test_stream = generate_struct(&simple_struct).unwrap();
        assert_eq!("struct Test { flag : bool }", test_stream.to_string());
    }

    // Return test
    #[test]
    fn test_simple_return() {
        // return hello
        let simple_return = Return::new(Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(
            Ident::new("hello", None),
        )))));
        let test_stream = generate_return(&simple_return).unwrap();
        assert_eq!("return hello ;", test_stream.to_string());
    }

    // Comp expr test
    #[test]
    fn test_simple_comp() {
        // count > prev_count
        let comp_expr = Comp::new(
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "count", None,
            ))))),
            CompOpKind::Gt,
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "prev_count",
                None,
            ))))),
        );
        let test_stream = generate_comp(&comp_expr).unwrap();
        assert_eq!("count > prev_count", test_stream.to_string());
    }

    #[test]
    fn test_simple_array() {
        // [1, 2, 3, 4, 5]
        let array_expr = Array::new(vec![
            Lit::new(LitKind::NumberLit(NumberLit::new("1"))),
            Lit::new(LitKind::NumberLit(NumberLit::new("2"))),
            Lit::new(LitKind::NumberLit(NumberLit::new("3"))),
            Lit::new(LitKind::NumberLit(NumberLit::new("4"))),
            Lit::new(LitKind::NumberLit(NumberLit::new("5"))),
        ]);
        let test_stream = generate_array(&array_expr).unwrap();
        assert_eq!("vec ! [1 , 2 , 3 , 4 , 5]", test_stream.to_string());
    }

    #[test]
    fn test_simple_rule() {
        // rule hello<qn0>(q2: Qubit){expression}
        // let rule_expr = RuleExpr::new(
        //     Ident::new("hello", None),
        //     vec![Ident::new("qn0", None)],
        //     vec![Ident::new("q2", Some(TypeDef::Qubit))],
        //     vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
        //         Lit::new(LitKind::Ident(Ident::new("expression", None))),
        //     ))))],
        // );
        // let test_stream = generate_rule(&rule_expr).unwrap();
        // assert_eq!(
        //     "rule hello<qn0>(q2: Qubit){expression}",
        //     test_stream.to_string()
        // );
    }

    #[test]
    fn test_interface_generation() {
        // #interface: {qn0, qn1} => qnall;
    }
}
