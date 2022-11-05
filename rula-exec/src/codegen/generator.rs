// This is entory point to generate code from AST
use super::error::*;
use super::rule_meta::*;
use super::ruleset_generator::RuleSetFactory;
use super::IResult;

use crate::rulep::action::v2::ActionClauses;
use crate::rulep::action::Action;
use crate::rulep::condition::v1::*;
use crate::rulep::condition::Condition;
use crate::rulep::ruleset::{Rule, RuleSet};
use crate::wrapper::qnic_wrapper::QnicInterfaceWrapper;
use rula_parser::parser::ast::*;

use once_cell::sync::OnceCell;
use proc_macro2::{Span, TokenStream};
use std::collections::HashMap;
use std::sync::Mutex;
use syn::Lit as SynLit;
use syn::{LitFloat, LitStr};

// These are used but only in quote! macro
#[allow(unused_imports)]
use mock_components::hardware::result::MeasResult;
#[allow(unused_imports)]
use std::net::SocketAddr;

// Right now, compilation is all single thread. In the future, this could be multi thread.
// Mutex would be a good choice for that.
type MutexRuleSet = Mutex<RuleSet<ActionClauses>>;
static RULESET: OnceCell<MutexRuleSet> = OnceCell::new();

static RULESET_FACTORY: OnceCell<Mutex<RuleSetFactory>> = OnceCell::new();

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
    assert!(RULESET_FACTORY.get().is_none());

    // closure to initialize with empty ruleset
    let empty_ruleset = || Mutex::new(RuleSet::<ActionClauses>::new("empty_ruleset"));
    let _ = RULESET.get_or_init(empty_ruleset);

    let initialize_ruleset_factory = || Mutex::new(RuleSetFactory::init());
    let _ = RULESET_FACTORY.get_or_init(initialize_ruleset_factory);
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
    let ruleset_factory = RULESET_FACTORY
        .get()
        .expect("Failed to get RuleSet factory");

    for i in &interface.interface {
        ruleset_factory
            .lock()
            .unwrap()
            .add_global_interface(&i.name, QnicInterfaceWrapper::place_holder());
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
    let ruleset_factory = RULESET_FACTORY
        .get()
        .expect("Failed to get Ruleset factory");
    let (mut identifier, mut expr) = (quote!(), quote!());
    if in_watch {
        // Register values to watched_values in RuleMeta
        match rule {
            Some(name) => {
                // Don't wanna clone the expression here
                ruleset_factory.lock().unwrap().add_watch_value(
                    name,
                    &*let_stmt.ident.name,
                    Watchable::UnSet,
                );
                identifier = generate_ident(&*let_stmt.ident).unwrap();
                expr = generate_expr(&*let_stmt.expr, rule).unwrap();
            }
            None => {
                todo!("No rule found")
            }
        }
    } else {
        identifier = generate_ident(&*let_stmt.ident).unwrap();
        expr = generate_expr(&*let_stmt.expr, rule).unwrap();
    }
    Ok(quote!(
        let mut #identifier = #expr;
    ))
}

fn generate_expr(
    expr: &Expr,
    rule: Option<&String>,
    // watched_value: Option<&String>,
) -> IResult<TokenStream> {
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
        ExprKind::Comp(comp_expr) => Ok(generate_comp(&comp_expr, rule).unwrap()),
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
                let new_ident = Ident::new(sp, None, IdentType::Other);
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
    // TODO: register this function to function name list and make it checkable
    // Here could have generics in the future
    Ok(quote!(
        pub fn ( #(#arguments),* ) {
            #stmt
        }
    ))
}
fn generate_fn_call(fn_call_expr: &FnCall, rule_name: Option<&String>) -> IResult<TokenStream> {
    // Before generating functions, check function table to check whether it's properly defined or not
    let fn_name = generate_ident(&fn_call_expr.func_name).unwrap();
    let generated_arguments = {
        let mut args = vec![];
        for arg in &fn_call_expr.arguments {
            args.push(generate_expr(arg, rule_name).unwrap());
        }
        args
    };
    let (in_rule, r_name) = helper::check_in(rule_name);
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

fn generate_comp(comp_expr: &Comp, rule_name: Option<&String>) -> IResult<TokenStream> {
    let lhs = generate_expr(&comp_expr.lhs, rule_name).unwrap();
    let rhs = generate_expr(&comp_expr.rhs, rule_name).unwrap();
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
    // 1. Generate static RuleSet for serialized output
    // Get meta information for this RuleSet
    let ruleset_name = &*ruleset_expr.name.name;

    let ruleset_factory = RULESET_FACTORY
        .get()
        .expect("Failed to get Ruleset factory");
    // Static RuleSet that can be output of this compiler
    let glob_ruleset = RULESET.get().expect("Failed to get ruleset");
    glob_ruleset.lock().unwrap().update_name(ruleset_name);

    // Closure that gets rule_name and evaluate if the rule is inside the table or not
    // If that Rule is in the table, add it to RuleSet
    let rule_add_evaluater = |rule_name: &String| {
        if !ruleset_factory.lock().unwrap().exist_rule(rule_name) {
            return Err(RuLaCompileError::NoRuleFoundError);
        } else {
            let corr_rule = ruleset_factory.lock().unwrap().get_rule(rule_name);
            glob_ruleset.lock().unwrap().add_rule(corr_rule);
            Ok(())
        }
    };

    for rule in &ruleset_expr.rules {
        match rule {
            // Ordinary Rule call (e.g. swapping_rule())
            RuleIdentifier::FnCall(fn_call_expr) => {
                // Rule without any return values
                rule_add_evaluater(&*fn_call_expr.func_name.name).unwrap();
            }
            RuleIdentifier::Let(let_stmt) => {
                // Rule with return value
                // In the static RuleSet, it doesn't care if there is a return value or not
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
    // Replace all the TBD information with configed values
    // Expand Config here
    let config = &*ruleset_expr.config;

    // 2. Generate RuleSet executable here
    Ok(quote!())
}

fn generate_rule(rule_expr: &RuleExpr) -> IResult<TokenStream> {
    // 1. Generate Rules and store them to the table

    let ruleset_factory = RULESET_FACTORY
        .get()
        .expect("Failed to get ruleset facotyr");

    // Get the basis information of Rule
    let rule_name = &*rule_expr.name;
    let rule_name_string = String::from(&*rule_name.name);

    // Check if there is a Rule with the same name
    if ruleset_factory.lock().unwrap().exist_rule(&*rule_name.name) {
        return Err(RuLaCompileError::RuleDuplicationError);
    }

    // Prepare an empty Rule
    let mut rule = Rule::<ActionClauses>::new(&rule_name.name);

    // When the ruleset is finalized, this interface name is replaced by actual interface name
    // Setup interface placeholder
    for interface in &rule_expr.interface {
        // Interface wrapper
        match &*interface.ident_type {
            IdentType::QnicInterface => {
                if !ruleset_factory
                    .lock()
                    .unwrap()
                    .exist_interface(&*interface.name)
                {
                    return Err(RuLaCompileError::NoInterfaceFoundError);
                }
                let target_interface = ruleset_factory
                    .lock()
                    .unwrap()
                    .get_interface(&*interface.name);
                // Providing ref would be better
                // This can be just a reference to the interface
                rule.add_interface(&interface.name, target_interface);
            }
            _ => return Err(RuLaCompileError::UnknownError),
        }
    }

    // Set empty condition and action to be updated in the different functions
    // Clauses are added directly to this empty condition and action
    let empty_condition = Condition::new(None);
    let empty_action = Action::<ActionClauses>::new(None);
    rule.set_condition(empty_condition);
    rule.set_action(empty_action);

    ruleset_factory
        .lock()
        .unwrap()
        .add_rule(&rule_name_string, rule);
    for arg in &rule_expr.args {
        ruleset_factory
            .lock()
            .unwrap()
            .add_rule_arg(&rule_name_string, &arg.name);
    }

    // 2. Generate executable Rule
    // generate the rule content expression with this
    let generated =
        generate_rule_content(&rule_expr.rule_content, Some(&rule_name_string)).unwrap();

    // RuLa runtime generation
    let rule_name_token = generate_ident(rule_name).unwrap();

    Ok(quote!(
        struct #rule_name_token{
            #generated
        }
    ))
}

fn generate_rule_content(
    rule_content_expr: &RuleContentExpr,
    rule: Option<&String>,
) -> IResult<TokenStream> {
    let watch_expr = &*rule_content_expr.watch_expr;
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
    let mut post_processes = vec![];
    for stmt in post_process {
        post_processes.push(generate_stmt(stmt, None).unwrap());
    }
    // Generate executable
    Ok(quote!(
        #generated_watch
        #generated_cond
        #generated_act
        #(#post_processes)*
    ))
}

fn generate_watch(watch_expr: &WatchExpr, rule_name: Option<&String>) -> IResult<TokenStream> {
    // Watch expression defines the set of parameters whose state can be changed in the future
    // e.g.
    // watch:
    //      let qubit = qn0.get_qubit();

    let mut generated_watch = vec![];
    match rule_name {
        Some(name) => {
            for value in &watch_expr.watched_values {
                // start watch value generation process with `in_watch=true`
                generated_watch.push(generate_let(value, Some(&name), true).unwrap());
                // watch can be
            }
        }
        None => todo!(),
    };

    // Generate
    // This could be on the tokio async runtime
    Ok(quote!(
        #(#generated_watch)*
    ))
}

fn generate_cond(cond_expr: &CondExpr, rule_name: Option<&String>) -> IResult<TokenStream> {
    let mut generated_clauses = vec![];
    let (in_rule, r_name) = helper::check_in(rule_name);
    // Sweep Rule meta here to get the condition clause information and add it
    // At this moment all information that needs to be considered should be inside the RuleMeta
    // However, some of them are given by configs.
    if in_rule {
        for clause in &cond_expr.clauses {
            generated_clauses.push(generate_awaitable(&clause, Some(&r_name)).unwrap());
        }
    } else {
        for clause in &cond_expr.clauses {
            generated_clauses.push(generate_awaitable(&clause, None).unwrap());
        }
    }

    Ok(quote!(
        #(#generated_clauses)*
    ))
}

fn generate_awaitable(
    awaitable_expr: &Awaitable,
    rule_name: Option<&String>,
) -> IResult<TokenStream> {
    // Generating condition clauses to be met
    let ruleset_facotry = RULESET_FACTORY.get().expect("Failed to get Rule Table");
    let (in_rule, r_name) = helper::check_in(rule_name);
    let mut awaitables = vec![];
    // Start creating a new condition and replace the old one with new condition?
    if in_rule {
        match awaitable_expr {
            Awaitable::FnCall(fn_call) => {
                // This should be flex

                awaitables.push(generate_fn_call(fn_call, rule_name).unwrap())
            }
            Awaitable::VariableCallExpr(val_call) => {
                // Check if the value is properly watched or not
                awaitables.push(generate_variable_call(val_call, Some(&r_name)).unwrap());
            }
            Awaitable::Comp(comp) => {
                awaitables.push(generate_comp(comp, rule_name).unwrap());
            }
        }
    } else {
        todo!("Currently, not ruleset generation version has not yet been supported")
    }
    Ok(quote!())
}

fn generate_act(act_expr: &ActExpr, rule: Option<&String>) -> IResult<TokenStream> {
    Ok(quote!())
}

fn generate_variable_call(
    variable_call_expr: &VariableCallExpr,
    rule_name: Option<&String>,
    // watched_value: Option<&String>,
) -> IResult<TokenStream> {
    // Check status (in_rule, in_watch)
    let (in_rule, r_name) = helper::check_in(rule_name);
    let ruleset_factory = RULESET_FACTORY
        .get()
        .expect("Failed to get Ruleset factory");

    // Start generating
    let mut val_calls = vec![];
    // if the first variable is interface name, that supposed to be interface function call
    let mut eval_first_two = |rule_name: Option<&String>| {
        match &variable_call_expr.variables[0] {
            Callable::FnCall(fn_call) => {
                val_calls.push(generate_fn_call(fn_call, rule_name).unwrap())
            }
            Callable::Ident(identifier) => {
                // if this variable call starts from interface name
                match &*identifier.ident_type {
                    IdentType::QnicInterface => {
                        if !ruleset_factory
                            .lock()
                            .unwrap()
                            .exist_interface(&*identifier.name)
                        {
                            // Here should not be panic!()
                            // return Err(RuLaCompileError::NoInterfaceFoundError);
                            panic!("No interface definition {} found", &*identifier.name);
                        }
                        match &variable_call_expr.variables[1] {
                            Callable::FnCall(builtin_qnic_fn) => {
                                // interface_table
                                //     .lock()
                                //     .unwrap()
                                //     .get_mut(&*identifier.name)
                                //     .unwrap()
                                //     .builtin_functions(builtin_qnic_fn);
                            }
                            _ => todo!("Qnic interface can only take functions right now"),
                        }
                    }
                    IdentType::WatchedVal => {
                        if !ruleset_factory
                            .lock()
                            .unwrap()
                            .exist_watched_value(&r_name, &*identifier.name)
                        {
                            panic!("No variable found in watch table.")
                        }
                        if in_rule {
                            ruleset_factory.lock().unwrap().update_watched_value(
                                &r_name,
                                &*identifier.name,
                                QuantumProp::place_holder(),
                            );
                        } else {
                            todo!()
                        }
                    }
                    _ => {
                        // If this is not a qnic interface or watched value, just treat as usual identifier
                        val_calls.push(generate_ident(identifier).unwrap());
                        match &variable_call_expr.variables[1] {
                            Callable::FnCall(fn_call_expr) => {
                                val_calls.push(generate_fn_call(fn_call_expr, rule_name).unwrap());
                            }
                            Callable::Ident(ident_expr) => {
                                val_calls.push(generate_ident(ident_expr).unwrap());
                            }
                        }
                    }
                }
            }
        }
    };

    match rule_name {
        Some(name) => {
            eval_first_two(Some(name));
            // FIXME: should combine these or do recursively
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
        LitKind::StringLit(string_lit) => {
            let val = SynLit::Str(LitStr::new(&string_lit.as_string(), Span::call_site()));
            Ok(quote!(#val))
        }
        _ => todo!("{:#?} not yet implemented", lit),
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

pub mod helper {
    pub fn check_in(named: Option<&String>) -> (bool, String) {
        match named {
            Some(name) => (true, name.clone()),
            None => (false, String::from("")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ident tests
    #[test]
    fn test_ident_no_type_hint() {
        let test_ident = Ident::new("hello", None, IdentType::Other);
        let test_stream = generate_ident(&test_ident).unwrap();
        assert_eq!("hello", test_stream.to_string());
    }
    #[test]
    fn test_ident_with_type_hint() {
        let test_ident = Ident::new("hello", Some(TypeDef::Boolean), IdentType::Other);
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
                "block",
                None,
                IdentType::Other,
            ))))),
            // {expression}
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("expression", None, IdentType::Other)),
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
                "block",
                None,
                IdentType::Other,
            ))))),
            // {expression}
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("expression", None, IdentType::Other)),
            ))))),
            // elif ~
            None,
            // else ~
            Some(Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
                Lit::new(LitKind::Ident(Ident::new(
                    "expression2",
                    None,
                    IdentType::Other,
                ))),
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
                "block",
                None,
                IdentType::Other,
            ))))),
            // {expression}
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("expression", None, IdentType::Other)),
            ))))),
            // elif ~
            Some(If::new(
                // else if (block)
                Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                    "block2",
                    None,
                    IdentType::Other,
                ))))),
                // else if () {statement2;};
                Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                    LitKind::Ident(Ident::new("expression2", None, IdentType::Other)),
                ))))),
                None,
                None,
            )),
            // else ~
            Some(Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
                Lit::new(LitKind::Ident(Ident::new(
                    "expression3",
                    None,
                    IdentType::Other,
                ))),
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
            vec![Ident::new("i", None, IdentType::Other)],
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "generator",
                None,
                IdentType::Other,
            ))))),
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("hello", None, IdentType::Other)),
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
                Ident::new("a", None, IdentType::Other),
                Ident::new("b", None, IdentType::Other),
                Ident::new("c", None, IdentType::Other),
            ],
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "generator",
                None,
                IdentType::Other,
            ))))),
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("hello", None, IdentType::Other)),
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
                "count",
                None,
                IdentType::Other,
            ))))),
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("expression", None, IdentType::Other)),
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
                Ident::new("block", Some(TypeDef::Integer32), IdentType::Other),
                Ident::new("hello", Some(TypeDef::Str), IdentType::Other),
            ],
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("expression", None, IdentType::Other)),
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
        let simple_fn_call = FnCall::new(Ident::new("range", None, IdentType::Other), vec![]);
        let test_stream = generate_fn_call(&simple_fn_call, None).unwrap();
        assert_eq!("range ()", test_stream.to_string());
    }

    // Struct test
    #[test]
    fn test_simple_struct() {
        // struct Test{flag: bool}
        let simple_struct = Struct::new(
            Ident::new("Test", None, IdentType::Other),
            vec![Ident::new("flag", Some(TypeDef::Boolean), IdentType::Other)],
        );
        let test_stream = generate_struct(&simple_struct).unwrap();
        assert_eq!("struct Test { flag : bool }", test_stream.to_string());
    }

    // Return test
    #[test]
    fn test_simple_return() {
        // return hello
        let simple_return = Return::new(Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(
            Ident::new("hello", None, IdentType::Other),
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
                "count",
                None,
                IdentType::Other,
            ))))),
            CompOpKind::Gt,
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "prev_count",
                None,
                IdentType::Other,
            ))))),
        );
        let test_stream = generate_comp(&comp_expr, None).unwrap();
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
