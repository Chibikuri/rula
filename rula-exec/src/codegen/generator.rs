// This is entory point to generate code from AST
use super::error::*;
use super::identifier::*;
use super::rule_meta::*;
use super::ruleset_generator::RuleSetFactory;
use super::IResult;

use crate::rulep::action::v2::ActionClauses;
use crate::rulep::action::Action;
use crate::rulep::condition::Condition;
use crate::rulep::ruleset::{Rule, RuleSet};
use crate::wrapper::qnic_wrapper::QnicInterfaceWrapper;
use rula_parser::parser::ast::*;

use once_cell::sync::OnceCell;
use proc_macro2::{Span, TokenStream};
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
static RULES: OnceCell<Mutex<Vec<String>>> = OnceCell::new();

static RULESET_FACTORY: OnceCell<Mutex<RuleSetFactory>> = OnceCell::new();

/// Generate corresponding rust code from ast
/// Every nested generators returns a piece of TokenStream
pub fn generate(ast_tree: &mut Vec<AstNode>, with_ruleset: bool) -> IResult<TokenStream> {
    // initialize all the global values (RULESET, RULE_TABLE)
    initialize_singleton();

    let rules = RULES.get().expect("Unable to get rule table");

    // RuLa starts here
    if ast_tree.len() > 1 {
        todo!("This should get a root AST node. Fix it");
    }

    // Generated rula program
    let rula_program = match &mut ast_tree[0] {
        AstNode::RuLa(rula) => generate_rula(rula).unwrap(),
        AstNode::PlaceHolder => {
            return Err(RuLaCompileError::RuLaInitializationError(
                InitializationError::new("at very first generation"),
            ))
        }
    };
    let generated_tests = helper::generate_test();

    let mut rule_names = vec![];
    for rname in rules.lock().unwrap().iter() {
        let rname_ident = format_ident!("{}", rname);
        rule_names.push(quote!(
            ruleset.add_rule(Box::new(rula::#rname_ident::new()))
        ))
    }
    let ruleset_gen = if with_ruleset {
        quote!(ruleset.gen_ruleset();)
    } else {
        quote!()
    };
    // All ast are supposed to be evaluated here
    let rula_token_stream = quote!(
        use rula_lib as rula_std;
        #[allow(unused)]
        mod rula{
            use super::*;
            use rula_std::rule::*;
            use rula_std::prelude::*;
            use async_trait::async_trait;
            #rula_program
        }
        pub async fn main(){
            rula::initialize_interface().await;
            let mut ruleset = rula::RuleSet::init();
            #(#rule_names);*;
            #ruleset_gen
        }

        #[cfg(test)]
        mod tests{
            use super::*;
            use super::rula::*;
            #generated_tests
        }
    );
    Ok(rula_token_stream)
}

// Initialize singleton values
// RULESET_FACTORY: collect information needed to create a RuleSet
// RULESET: ruleset instance (will be deprecated in generator)
fn initialize_singleton() {
    // Set the initial RuleSet so that other RuleSet call can only get without init
    assert!(RULESET_FACTORY.get().is_none());
    assert!(RULES.get().is_none());

    let initialize_ruleset_factory = || Mutex::new(RuleSetFactory::init());
    let _ = RULESET_FACTORY.get_or_init(initialize_ruleset_factory);

    let initialize_rule = || Mutex::new(vec![]);
    let _ = RULES.get_or_init(initialize_rule);
}

// Generate entire rula program
fn generate_rula(rula: &mut RuLa) -> IResult<TokenStream> {
    match &mut *rula.rula {
        RuLaKind::Program(program) => {
            let generated_program = generate_program(program).unwrap();
            return Ok(quote!(#generated_program));
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

// Generate progra
// program: Program AST that contains a vector of Stmt
fn generate_program(program: &mut Program) -> IResult<TokenStream> {
    // ident_tracker tracks all the identifier especially special values
    let mut ident_tracker = IdentTracker::new();

    // A vector to store a set of generated stmts
    let mut stmts = vec![];
    for program_block in &mut program.programs {
        // Right now, a program can takes a stmt
        match program_block {
            ProgramKind::Stmt(stmt) => {
                stmts.push(generate_stmt(stmt, None, &mut ident_tracker).unwrap())
            }
        }
    }
    Ok(quote!(#(#stmts )*))
}

// Generate stmt expression
// Arguments:
//  stmt: Stmt AST
//  rule_name: Option<String> a name of corresponding Rule
//  ident_tracker: List of identifiers that need to be tracked
fn generate_stmt(
    stmt: &mut Stmt,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let generated_stmt = match &mut *stmt.kind {
        StmtKind::Let(let_stmt) => {
            Ok(generate_let(let_stmt, rule_name, false, ident_tracker).unwrap())
        }
        StmtKind::Interface(interface) => Ok(generate_interface(interface, ident_tracker).unwrap()),
        StmtKind::Config(config) => Ok(generate_config(config, ident_tracker).unwrap()),
        StmtKind::Expr(expr) => Ok(generate_expr(expr, rule_name, ident_tracker).unwrap()),
        StmtKind::PlaceHolder => Err(RuLaCompileError::RuLaInitializationError(
            InitializationError::new("at generate rula"),
        )),
    }
    .unwrap();
    Ok(quote!(#generated_stmt))
}

/// Generate config schema
fn generate_config(
    config_stmt: &mut Config,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let config_name = generate_ident(&*config_stmt.name, ident_tracker, false).unwrap();
    let mut generated_items = vec![];

    for item in &mut *config_stmt.values {
        generated_items.push(generate_config_item(item, ident_tracker).unwrap())
    }
    Ok(quote!(
        pub struct #config_name{
            #(#generated_items),*
        }
    ))
}

fn generate_config_item(
    config_item: &mut ConfigItem,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let value_name = generate_ident(&*config_item.value, ident_tracker, false).unwrap();
    let type_def = generate_type_hint(&*config_item.type_def).unwrap();
    Ok(quote!(#value_name: #type_def))
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
fn generate_let(
    let_stmt: &mut Let,
    rule_name: Option<&String>,
    in_watch: bool,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    // RuleSet singleton to store ruleset information
    let ruleset_factory = RULESET_FACTORY
        .get()
        .expect("Failed to get Ruleset factory");
    let (mut identifier, mut expr) = (quote!(), quote!());

    // If this let stmt is in the watch clause
    if in_watch {
        // Register values to watched_values in RuleMeta
        match rule_name {
            Some(name) => {
                // Don't wanna clone the expression here
                ruleset_factory.lock().unwrap().add_watch_value(
                    name,
                    &*let_stmt.ident.name,
                    Watchable::UnSet,
                );
                ident_tracker.register(
                    &*let_stmt.ident.name,
                    Identifier::new(IdentType::WatchedValue, TypeHint::Unknown),
                );
                match &*let_stmt.ident.type_hint {
                    Some(hint) => {
                        match hint {
                            TypeDef::Qubit => ident_tracker.register(
                                &*let_stmt.ident.name,
                                Identifier::new(IdentType::QubitInterface, TypeHint::Qubit),
                            ),
                            _ => {}
                        }
                        identifier = generate_ident(&*let_stmt.ident, ident_tracker, true).unwrap();
                    }
                    None => {
                        identifier =
                            generate_ident(&*let_stmt.ident, ident_tracker, false).unwrap();
                    }
                }
                expr = generate_expr(&mut *let_stmt.expr, rule_name, ident_tracker).unwrap();
            }
            None => {
                todo!("No rule found")
            }
        }
    } else {
        match &*let_stmt.ident.type_hint {
            Some(hint) => {
                match hint {
                    TypeDef::Qubit => ident_tracker.register(
                        &*let_stmt.ident.name,
                        Identifier::new(IdentType::QubitInterface, TypeHint::Qubit),
                    ),
                    _ => {}
                }
                identifier = generate_ident(&*let_stmt.ident, ident_tracker, true).unwrap();
            }
            None => {
                identifier = generate_ident(&*let_stmt.ident, ident_tracker, false).unwrap();
            }
        }
        expr = generate_expr(&mut *let_stmt.expr, rule_name, ident_tracker).unwrap();
    }
    // This is also naive implementation
    if in_watch {
        Ok(quote!(
            let mut interface = INTERFACES
            .get()
            .expect("unable to get interface")
            .lock().await;
            let mut #identifier = #expr.await;
        ))
    } else {
        Ok(quote!(
            let mut interface = INTERFACES
            .get()
            .expect("unable to get interface")
            .lock().await;
            let mut #identifier = #expr;
        ))
    }
}

fn generate_interface(
    interface_expr: &mut Interface,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let mut interface_names = vec![];
    let ruleset_factory = RULESET_FACTORY
        .get()
        .expect("Failed to get RuleSet factory");

    for i in &mut interface_expr.interface {
        ruleset_factory
            .lock()
            .unwrap()
            .add_global_interface(&i.name, QnicInterfaceWrapper::place_holder());
        // TODO: clean up
        ident_tracker.register(
            &i.name,
            Identifier::new(IdentType::QnicInterface, TypeHint::Unknown),
        );
        i.update_ident_type(IdentType::QnicInterface);
        interface_names.push(SynLit::Str(LitStr::new(&i.name, Span::call_site())));
    }
    let interface_group_name = match &mut *interface_expr.group_name {
        Some(group_name) => {
            group_name.update_ident_type(IdentType::QnicInterface);
            &*group_name.name
        }
        None => "",
    };
    interface_names.push(SynLit::Str(LitStr::new(
        interface_group_name,
        Span::call_site(),
    )));
    ident_tracker.register(
        interface_group_name,
        Identifier::new(IdentType::QnicInterface, TypeHint::Unknown),
    );
    Ok(quote!(
        use std::collections::HashMap;
        use tokio::sync::Mutex;
        use once_cell::sync::OnceCell;
        use rula_std::qnic::QnicInterface;
        use rula_std::qubit::QubitInterface;
        pub static INTERFACES: OnceCell<Mutex<HashMap<String, QnicInterface>>> = OnceCell::new();

        pub async fn initialize_interface() {
            assert!(INTERFACES.get().is_none());
            let initialize_interface = || Mutex::new(HashMap::new());
            INTERFACES.get_or_init(initialize_interface);
            let interface_list = INTERFACES.get().expect("Failed to get interface");
            for interface_name in vec![#(#interface_names),*] {
                interface_list
                    .lock()
                    .await
                    .insert(interface_name.to_string(), QnicInterface::place_holder());
            }
        }
    ))
}

fn generate_expr(
    expr: &mut Expr,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
    // watched_value: Option<&String>,
) -> IResult<TokenStream> {
    match &mut *expr.kind {
        ExprKind::Import(import_expr) => Ok(generate_import(import_expr, ident_tracker).unwrap()),
        ExprKind::If(if_expr) => Ok(generate_if(if_expr, ident_tracker).unwrap()),
        ExprKind::For(for_expr) => Ok(generate_for(for_expr, ident_tracker).unwrap()),
        ExprKind::While(while_expr) => Ok(generate_while(while_expr, ident_tracker).unwrap()),
        ExprKind::FnDef(fn_def_expr) => Ok(generate_fn_def(fn_def_expr, ident_tracker).unwrap()),
        ExprKind::FnCall(fn_call_expr) => {
            Ok(generate_fn_call(fn_call_expr, rule_name, ident_tracker, false).unwrap())
        }
        ExprKind::Struct(struct_expr) => Ok(generate_struct(struct_expr, ident_tracker).unwrap()),
        ExprKind::Return(return_expr) => Ok(generate_return(return_expr, ident_tracker).unwrap()),
        ExprKind::Match(match_expr) => Ok(generate_match(match_expr, ident_tracker).unwrap()),
        ExprKind::Comp(comp_expr) => {
            Ok(generate_comp(comp_expr, rule_name, ident_tracker).unwrap())
        }
        ExprKind::RuleSetExpr(ruleset_expr) => {
            Ok(generate_ruleset_expr(ruleset_expr, ident_tracker).unwrap())
        }
        ExprKind::RuleExpr(rule_expr) => Ok(generate_rule(rule_expr, ident_tracker).unwrap()),
        ExprKind::CondExpr(cond_expr) => {
            Ok(generate_cond(cond_expr, rule_name, ident_tracker, quote!()).unwrap())
        }
        ExprKind::ActExpr(act_expr) => {
            Ok(generate_act(act_expr, rule_name, ident_tracker).unwrap())
        }
        ExprKind::VariableCallExpr(variable_call_expr) => {
            Ok(generate_variable_call(variable_call_expr, rule_name, ident_tracker).unwrap())
        }
        ExprKind::Array(array_expr) => Ok(generate_array(&array_expr, ident_tracker).unwrap()),
        ExprKind::Lit(lit_expr) => Ok(generate_lit(&lit_expr, ident_tracker).unwrap()),
        ExprKind::Term(term_expr) => Ok(generate_term(*term_expr).unwrap()),
        ExprKind::PlaceHolder => Err(RuLaCompileError::RuLaInitializationError(
            InitializationError::new("at generating expression"),
        )),
    }
}

fn generate_import(
    import_expr: &mut Import,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
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
                let mut top_path = "";
                if sp == "std" {
                    top_path = "rula_std";
                } else {
                    top_path = sp;
                }
                let new_ident = Ident::new(top_path, None, IdentType::Other);
                let path_fragment = generate_ident(&new_ident, ident_tracker, false).unwrap();
                single_path.push(path_fragment)
            }
            let path_head = &single_path[0];
            let path_left = &single_path[1..];
            quote!(
                #path_head #(::#path_left)*
            )
        } else {
            let path_ident = generate_ident(path_ident, ident_tracker, false).unwrap();
            quote!(#path_ident)
        };
        paths.push(quoted_path);
    }
    Ok(quote!(
        #(use #paths; )*
    ))
}

fn generate_if(if_expr: &mut If, ident_tracker: &mut IdentTracker) -> IResult<TokenStream> {
    // block could have invalid expression here.
    let block_quote = generate_expr(&mut *if_expr.block, None, ident_tracker).unwrap();
    let stmt_quote = generate_stmt(&mut *if_expr.stmt, None, ident_tracker).unwrap();
    if if_expr.elif.len() > 0 {
        // no elif statement
        if if_expr.elif[0] == None {
            match &mut *if_expr.els {
                // With else statement
                Some(els_stmt) => {
                    let els_stmt_quote = generate_stmt(els_stmt, None, ident_tracker).unwrap();
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
            for elif_expr in &mut *if_expr.elif {
                match elif_expr {
                    Some(if_expr) => elif_quotes.push(generate_if(if_expr, ident_tracker).unwrap()),
                    None => {
                        unreachable!()
                    }
                }
            }

            match &mut *if_expr.els {
                Some(els_stmt) => {
                    let els_stmt_quote = generate_stmt(els_stmt, None, ident_tracker).unwrap();
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

fn generate_for(for_expr: &mut For, ident_tracker: &mut IdentTracker) -> IResult<TokenStream> {
    let mut ident_list = vec![];
    for ident in for_expr.pattern.iter() {
        ident_list.push(generate_ident(ident, ident_tracker, false).unwrap());
    }
    let generator = generate_expr(&mut for_expr.generator, None, ident_tracker).unwrap();
    let stmt = generate_stmt(&mut for_expr.stmt, None, ident_tracker).unwrap();
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

fn generate_while(
    while_expr: &mut While,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let block = generate_expr(&mut while_expr.block, None, ident_tracker).unwrap();
    let stmt = generate_stmt(&mut while_expr.stmt, None, ident_tracker).unwrap();
    Ok(quote!(
        while #block{
            #stmt
        }
    ))
}

fn generate_fn_def(
    fn_def_expr: &mut FnDef,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let mut arguments = vec![];
    for ident in fn_def_expr.arguments.iter() {
        match &*ident.type_hint {
            Some(hint) => {
                arguments.push(generate_ident(ident, ident_tracker, true).unwrap());
            }
            None => {
                arguments.push(generate_ident(ident, ident_tracker, false).unwrap());
            }
        }
    }
    let stmt = generate_stmt(&mut fn_def_expr.stmt, None, ident_tracker).unwrap();
    // TODO: register this function to function name list and make it checkable
    // Here could have generics in the future
    Ok(quote!(
        pub fn ( #(#arguments),* ) {
            #stmt
        }
    ))
}

fn generate_fn_call(
    fn_call_expr: &mut FnCall,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
    do_await: bool,
) -> IResult<TokenStream> {
    // Before generating functions, check function table to check whether it's properly defined or not
    let fn_name = generate_ident(&fn_call_expr.func_name, ident_tracker, false).unwrap();
    let generated_arguments = {
        let mut args = vec![];
        for arg in &mut fn_call_expr.arguments {
            args.push(generate_expr(arg, rule_name, ident_tracker).unwrap());
        }
        args
    };
    if do_await {
        Ok(quote!(
            #fn_name(#(#generated_arguments), *).await
        ))
    } else {
        Ok(quote!(
            #fn_name (#(#generated_arguments),*)
        ))
    }
}

fn generate_struct(struct_expr: &Struct, ident_tracker: &mut IdentTracker) -> IResult<TokenStream> {
    // todo!("Structure definition would be deprecated");
    let struct_name = generate_ident(&struct_expr.name, ident_tracker, false).unwrap();
    let mut struct_items = vec![];
    for ident in struct_expr.items.iter() {
        struct_items.push(generate_ident(ident, ident_tracker, true).unwrap());
    }
    Ok(quote!(
        struct #struct_name{
            #(#struct_items),*
        }
    ))
}

fn generate_return(
    return_expr: &mut Return,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let expr = generate_expr(&mut return_expr.target, None, ident_tracker).unwrap();
    Ok(quote!(
        return #expr;
    ))
}

fn generate_match(
    match_expr: &mut Match,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let generated_expr = generate_expr(&mut match_expr.expr, None, ident_tracker).unwrap();
    let mut match_arms = vec![];

    for arm in &mut match_expr.match_arms {
        match_arms.push(generate_match_arm(arm, ident_tracker).unwrap());
    }

    let finally = match &mut *match_expr.finally {
        Some(fin) => generate_match_action(fin, ident_tracker).unwrap(),
        None => quote!(),
    };

    match &*match_expr.temp_val {
        Some(value) => {
            let temp_val = generate_ident(value, ident_tracker, false).unwrap();
            Ok(quote!(
                let #temp_val = #generated_expr;
                match #temp_val{
                    #(#match_arms),*
                    _ => {#finally}
                }
            ))
        }
        None => Ok(quote!(
            match #generated_expr{
                #(#match_arms),*
                _ => {#finally}
            }
        )),
    }
}

fn generate_match_arm(
    match_arm: &mut MatchArm,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let generated_match_condition =
        generate_match_condition(&mut *match_arm.condition, ident_tracker).unwrap();
    let generated_match_action =
        generate_match_action(&mut *match_arm.action, ident_tracker).unwrap();
    Ok(quote!(#generated_match_condition => {#generated_match_action}))
}

fn generate_match_condition(
    match_condition: &mut MatchCondition,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    // Right now, satisfiable can only take literals, but in the future, this should be more flexible
    match &mut *match_condition.satisfiable {
        Satisfiable::Lit(literal) => Ok(generate_lit(literal, ident_tracker).unwrap()),
        _ => Err(RuLaCompileError::RuLaInitializationError(
            InitializationError::new("at match condition"),
        )),
    }
}

fn generate_match_action(
    match_action: &mut MatchAction,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let mut actionables = vec![];
    for actionable in &mut *match_action.actionable {
        actionables.push(generate_expr(actionable, None, ident_tracker).unwrap());
    }
    Ok(quote!(
        #(#actionables);*
    ))
}

fn generate_comp(
    comp_expr: &mut Comp,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let lhs = generate_expr(&mut comp_expr.lhs, rule_name, ident_tracker).unwrap();
    let rhs = generate_expr(&mut comp_expr.rhs, rule_name, ident_tracker).unwrap();
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

fn generate_ruleset_expr(
    ruleset_expr: &RuleSetExpr,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    // 1. Generate static RuleSet for serialized output
    // Get meta information for this RuleSet
    let ruleset_name = &*ruleset_expr.name.name;

    let ruleset_factory = RULESET_FACTORY
        .get()
        .expect("Failed to get Ruleset factory");

    let rules = RULES.get().expect("Failed to get Rule table");

    // Closure that gets rule_name and evaluate if the rule is inside the table or not
    // If that Rule is in the table, add it to RuleSet
    for rule in &ruleset_expr.rules {
        match rule {
            // Ordinary Rule call (e.g. swapping_rule())
            RuleIdentifier::FnCall(fn_call_expr) => {
                // Rule without any return values
                // rule_add_evaluater(&*fn_call_expr.func_name.name).unwrap();
                rules
                    .lock()
                    .unwrap()
                    .push(String::from(&*fn_call_expr.func_name.name));
            }
            RuleIdentifier::Let(let_stmt) => {
                // Rule with return value
                // In the static RuleSet, it doesn't care if there is a return value or not
                // The name of the function must be in RULE_TABLE
                let expr = &*let_stmt.expr;
                match &*expr.kind {
                    ExprKind::FnCall(fn_call) => {
                        // rule_add_evaluater(&*fn_call.func_name.name).unwrap();
                        rules
                            .lock()
                            .unwrap()
                            .push(String::from(&*fn_call.func_name.name));
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
    Ok(quote!(
        pub struct RuleSet {
            rules: Vec<Box<dyn Rulable>>,
        }
        impl RuleSet {
            pub fn init() -> Self{
                RuleSet{
                    rules: vec![],
                }
            }
            pub fn add_rule(&mut self, rule: Box<dyn Rulable>){
                self.rules.push(rule);
            }

            pub fn gen_ruleset(&self){}
        }
    ))
}

fn generate_rule(
    rule_expr: &mut RuleExpr,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    // 1. Generate Rules and store them to the table

    let ruleset_factory = RULESET_FACTORY
        .get()
        .expect("Failed to get ruleset facotyr");

    // Get the basis information of Rule
    let rule_name = &*rule_expr.name;
    let rule_name_string = String::from(&*rule_name.name);

    if ruleset_factory.lock().unwrap().exist_rule(&*rule_name.name) {
        return Err(RuLaCompileError::RuleDuplicationError);
    }

    // Prepare an empty Rule
    let mut rule = Rule::<ActionClauses>::new(&rule_name.name);

    // When the ruleset is finalized, this interface name is replaced by actual interface name
    // Setup interface placeholder
    let mut interface_idents = vec![];
    for interface in &rule_expr.interface {
        // Interface wrapper
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
        let gen_str = SynLit::Str(LitStr::new(&*interface.name, Span::call_site()));
        interface_idents.push(quote!(#gen_str.to_string()));
        rule.add_interface(&interface.name, target_interface);
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

    let mut generated_args = vec![];
    for arg in &mut rule_expr.args {
        ruleset_factory
            .lock()
            .unwrap()
            .add_rule_arg(&rule_name_string, &arg.name);
        ident_tracker.register(
            &*arg.name,
            Identifier::new(IdentType::RuleArgument, helper::type_filler(&arg)),
        );
        arg.update_ident_type(IdentType::RuleArgument);
        generated_args.push(SynLit::Str(LitStr::new(&arg.name, Span::call_site())));
    }

    // 2. Generate executable Rule
    // generate the rule content expression with this
    let generated = generate_rule_content(
        &mut rule_expr.rule_content,
        Some(&rule_name_string),
        ident_tracker,
    )
    .unwrap();

    // RuLa runtime generation
    let rule_name_token = generate_ident(rule_name, ident_tracker, false).unwrap();

    Ok(quote!(
        pub struct #rule_name_token{
            interfaces: Vec<String>,
            arguments: HashMap<String, Argument>
        }
        impl #rule_name_token{
            pub fn new() -> Self{
                let mut argument_map = HashMap::new();
                for i in vec![#(#generated_args),*]{
                    argument_map.insert(i.to_string(), Argument::init());
                }
                #rule_name_token{
                    interfaces: vec![#(#interface_idents), *],
                    arguments: argument_map
                }
            }
        }
        #[async_trait]
        impl Rulable for #rule_name_token{
            #generated
        }

    ))
}

fn generate_rule_content(
    rule_content_expr: &mut RuleContentExpr,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    // Actions to be performed
    let generated_act = generate_act(
        &mut *rule_content_expr.action_expr,
        rule_name,
        ident_tracker,
    )
    .unwrap();
    let generated_cond = generate_cond(
        &mut *rule_content_expr.condition_expr,
        rule_name,
        ident_tracker,
        generated_act,
    )
    .unwrap();
    let post_process = &mut rule_content_expr.post_processing;
    let mut post_processes = vec![];
    for stmt in post_process {
        post_processes.push(generate_stmt(stmt, None, ident_tracker).unwrap());
    }
    // Generate executable
    // Here we should consider to use TaskMonitor in tokio to track the status of watched values
    Ok(quote!(
        async fn condition(&self) -> bool{
            #generated_cond
        }

        fn post_process(&self){
            #(#post_processes)*
        }
        // pub is implied in trait
        async fn execute(&self){
            loop{
                let done = self.condition().await;
                if done{
                    break;
                }
            }
        }
    ))
}

fn generate_watch(
    watch_expr: &mut WatchExpr,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    // Watch expression defines the set of parameters whose state can be changed in the future
    // e.g.
    // watch:
    //      let qubit = qn0.get_qubit();

    let mut generated_watch = vec![];
    // let mut watched_values = vec![];
    match rule_name {
        Some(name) => {
            for value in &mut watch_expr.watched_values {
                // start watch value generation process with `in_watch=true`
                // watched_values.push(generate_ident(&value.ident).unwrap());
                value.ident.update_ident_type(IdentType::WatchedValue);
                generated_watch
                    .push(generate_let(value, Some(&name), true, ident_tracker).unwrap());
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

fn generate_cond(
    cond_expr: &mut CondExpr,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
    act_tokens: TokenStream,
) -> IResult<TokenStream> {
    let mut generated_clauses = vec![];
    let (in_rule, r_name) = helper::check_in(rule_name);

    // Sweep Rule meta here to get the condition clause information and add it
    // At this moment all information that needs to be considered should be inside the RuleMeta
    // However, some of them are given by configs.
    if in_rule {
        for clause in &mut cond_expr.clauses {
            generated_clauses
                .push(generate_awaitable(clause, Some(&r_name), ident_tracker).unwrap());
        }
        match &mut *cond_expr.watch_expr {
            Some(watch_expr) => {
                let generated_watch =
                    generate_watch(watch_expr, Some(&r_name), ident_tracker).unwrap();
                Ok(quote!(
                   #generated_watch
                   if #(#generated_clauses)&&*{
                    // self.action().await;
                    (||async {
                        #act_tokens
                    })();
                    return true
                   }else{
                    return false
                   };
                ))
            }
            None => Ok(quote!(
               if #(#generated_clauses)&&*{
                (||async {
                    #act_tokens
                })();
                return true
               }else{
                return false
               };
            )),
        }
    } else {
        todo!()
    }
}

fn generate_awaitable(
    awaitable_expr: &mut Awaitable,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    // Generating condition clauses to be met
    let (in_rule, r_name) = helper::check_in(rule_name);
    // Start creating a new condition and replace the old one with new condition?
    let mut awaitable = quote!();
    if in_rule {
        awaitable = match awaitable_expr {
            Awaitable::FnCall(fn_call) => {
                // This should be flex
                let generated_fn_call =
                    generate_fn_call(fn_call, rule_name, ident_tracker, true).unwrap();
                quote!(
                    #generated_fn_call
                )
            }
            Awaitable::VariableCallExpr(val_call) => {
                // Check if the value is properly watched or not
                let generated_val =
                    generate_variable_call(val_call, Some(&r_name), ident_tracker).unwrap();
                quote!(
                    #generated_val.await
                )
            }
            Awaitable::Comp(comp) => {
                let generated_comp = generate_comp(comp, rule_name, ident_tracker).unwrap();
                quote!(
                    (||{ #generated_comp })()
                )
            }
        }
    } else {
        todo!("Currently, not ruleset generation version has not yet been supported")
    }
    Ok(quote!(
        #awaitable
    ))
}

fn generate_act(
    act_expr: &mut ActExpr,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let mut action_calls = vec![];

    for action in &mut act_expr.operatable {
        // TODO: Should integrate but for now, we need to know if there is ";" at the end
        match &mut *action.kind {
            StmtKind::Let(let_stmt) => {
                action_calls.push(generate_let(let_stmt, rule_name, false, ident_tracker).unwrap())
            }
            _ => {
                let generated = generate_stmt(action, rule_name, ident_tracker).unwrap();
                action_calls.push(quote!(
                    #generated;
                ))
            }
        }
    }
    Ok(quote!(
        #(#action_calls)*
    ))
}

fn generate_variable_call(
    variable_call_expr: &mut VariableCallExpr,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let mut variable_calls = vec![];

    if variable_call_expr.variables.len() == 0 {
        panic!("Internal Error. This needs to be treated in parser.")
    }
    for (index, val) in &mut variable_call_expr.variables.iter().enumerate() {
        let specified = if index == 0 {
            false
        } else {
            match &variable_call_expr.variables[index - 1] {
                Callable::Ident(ident) => {
                    // If the variable is registered as qubit, operations should be awaited
                    match ident_tracker.check_ident_type(&ident.name) {
                        IdentType::QubitInterface => true,
                        _ => false,
                    }
                }
                _ => false,
            }
        };

        match val.clone() {
            Callable::FnCall(mut fn_call_inner) => {
                if specified {
                    variable_calls.push(
                        generate_fn_call(&mut fn_call_inner, rule_name, ident_tracker, true)
                            .unwrap(),
                    );
                } else {
                    variable_calls.push(
                        generate_fn_call(&mut fn_call_inner, rule_name, ident_tracker, false)
                            .unwrap(),
                    );
                }
            }
            Callable::Ident(ident) => {
                variable_calls.push(generate_ident(&ident, ident_tracker, false).unwrap());
            }
        }
    }
    Ok(quote!(#(#variable_calls).*))
}

fn generate_array(array_expr: &Array, ident_tracker: &mut IdentTracker) -> IResult<TokenStream> {
    let mut items = vec![];
    for lits in array_expr.items.iter() {
        items.push(generate_lit(lits, ident_tracker).unwrap());
    }
    Ok(quote!(vec![#(#items),*]))
}

fn generate_lit(lit: &Lit, ident_tracker: &mut IdentTracker) -> IResult<TokenStream> {
    match &*lit.kind {
        LitKind::Ident(ident_lit) => Ok(generate_ident(ident_lit, ident_tracker, false).unwrap()),
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
fn generate_ident(
    ident: &Ident,
    ident_tracker: &mut IdentTracker,
    with_type_annotation: bool,
) -> IResult<TokenStream> {
    // If the identifier is not properly set, return error
    if &*ident == &Ident::place_holder() {
        return Err(RuLaCompileError::FailedToSetValueError);
    }

    let identifier = format_ident!("{}", *ident.name);
    let ident_contents = match ident_tracker.check_ident_type(&*ident.name) {
        IdentType::QnicInterface => {
            let ident_str = SynLit::Str(LitStr::new(&*ident.name, Span::call_site()));
            quote!(
                interface.get(#ident_str)
                .expect("unable to get interface")
            )
        }
        IdentType::WatchedValue => quote!(#identifier),
        IdentType::RuleArgument => {
            let ident_str = SynLit::Str(LitStr::new(&*ident.name, Span::call_site()));
            quote!(
                self.arguments.get(#ident_str).unwrap()
            )
        }
        _ => {
            quote!(#identifier)
        }
    };
    if with_type_annotation {
        match &*ident.type_hint {
            Some(hint) => {
                let type_hint = generate_type_hint(hint).unwrap();
                return Ok(quote!(#ident_contents:#type_hint));
            }
            None => return Err(RuLaCompileError::NoTypeFoundError),
        }
    } else {
        match ident_tracker.check_type_hint(&*ident.name) {
            TypeHint::Integer64 => Ok(quote!(#ident_contents.eval_int64())),
            TypeHint::UnsignedInteger64 => Ok(quote!(#ident_contents.eval_unsigned_int64())),
            TypeHint::Float64 => Ok(quote!(#ident_contents.eval_float64())),
            TypeHint::Str => Ok(quote!(#ident_contents.eval_str())),
            TypeHint::Boolean => Ok(quote!(#ident_contents.eval_bool())),
            TypeHint::Qubit => Ok(quote!(#ident_contents)),
            TypeHint::Vector => Ok(quote!(#ident_contents)),
            TypeHint::Unknown => Ok(quote!(#ident_contents)),
            _ => {
                todo!("{:#?}", ident)
            }
        }
    }
}

fn generate_type_hint(type_hint: &TypeDef) -> IResult<TokenStream> {
    match type_hint {
        TypeDef::Boolean => Ok(quote!(bool)),
        TypeDef::Integer32 => Ok(quote!(i32)),
        TypeDef::Integer64 => Ok(quote!(i64)),
        TypeDef::UnsignedInteger32 => Ok(quote!(u32)),
        TypeDef::UnsignedInteger64 => Ok(quote!(u64)),
        TypeDef::Str => Ok(quote!(String)),
        TypeDef::Qubit => Ok(quote!(&QubitInterface)),
        TypeDef::Vector(inner) => {
            let inner_type = generate_type_hint(inner).unwrap();
            Ok(quote!(Vec<#inner_type>))
        }
        TypeDef::PlaceHolder => {
            return Err(RuLaCompileError::RuLaInitializationError(
                InitializationError::new("at type hint generation"),
            ))
        }
        _ => todo!("type {:#?} not yet implemented", type_hint),
    }
}

pub mod helper {
    use super::*;
    use proc_macro2::TokenStream;

    pub fn check_in(named: Option<&String>) -> (bool, String) {
        match named {
            Some(name) => (true, name.clone()),
            None => (false, String::from("")),
        }
    }

    pub fn generate_test() -> TokenStream {
        quote!(
            #[doc = "This is generated for entanglement_swapping.rula"]
            #[tokio::test]
            async fn test_interface() {
                assert!(INTERFACES.get().is_none());
                rula::initialize_interface().await;
                let interface = INTERFACES.get().expect("Failed to get interface table");
                assert!(interface.lock().await.contains_key("qn0"));
                assert!(interface.lock().await.contains_key("qn1"));
            }

            #[tokio::test]
            async fn run_main() {
                main().await;
            }
        )
    }

    pub fn type_filler(ident: &Ident) -> TypeHint {
        match &*ident.type_hint {
            Some(hint) => {
                match hint {
                    // Convert to available type
                    TypeDef::Integer32 => TypeHint::Integer64,
                    TypeDef::Integer64 => TypeHint::Integer64,
                    TypeDef::Float32 => TypeHint::Float64,
                    TypeDef::Float64 => TypeHint::Float64,
                    TypeDef::Str => TypeHint::Str,
                    TypeDef::UnsignedInteger32 => TypeHint::UnsignedInteger64,
                    TypeDef::UnsignedInteger64 => TypeHint::UnsignedInteger64,
                    TypeDef::Boolean => TypeHint::Boolean,
                    _ => {
                        todo!("Other type conversion is not yet implemented {:#?}", hint)
                    }
                }
            }
            None => TypeHint::Unknown,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ident tests
    #[test]
    fn test_ident_no_type_hint() {
        initialize_singleton();
        let test_ident = Ident::new("hello", None, IdentType::Other);
        let mut tracker = IdentTracker::new();
        tracker.register(
            "hello",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        let test_stream = generate_ident(&test_ident, &mut tracker, false).unwrap();
        assert_eq!("hello", test_stream.to_string());
    }
    #[test]
    fn test_ident_with_type_hint() {
        let test_ident = Ident::new("hello", Some(TypeDef::Boolean), IdentType::Other);
        let mut tracker = IdentTracker::new();
        tracker.register(
            "hello",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        let test_stream = generate_ident(&test_ident, &mut tracker, true).unwrap();
        assert_eq!("hello : bool", test_stream.to_string());
    }

    // Import test
    #[test]
    fn test_simple_import() {
        // assert!(IDENT_TABLE.get().is_none());
        // let initialize_ident_table = || Mutex::new(HashMap::<String, IdentType>::new());
        // let _ = IDENT_TABLE.get_or_init(initialize_ident_table);
        // import hello;
        let expected_paths = vec![["hello"].iter().collect()];
        let mut test_import = Import::new(PathKind::from(expected_paths));
        let mut tracker = IdentTracker::new();
        tracker.register(
            "hello",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        let test_stream = generate_import(&mut test_import, &mut tracker).unwrap();
        assert_eq!("use hello ;", test_stream.to_string());
    }

    // If test
    #[test]
    fn test_simple_if() {
        // if (block) {expression}
        let mut simple_if = If::new(
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
        let mut tracker = IdentTracker::new();
        tracker.register(
            "block",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        tracker.register(
            "expression",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        let test_stream = generate_if(&mut simple_if, &mut tracker).unwrap();
        assert_eq!("if block { expression }", test_stream.to_string());
    }

    #[test]
    fn test_if_else() {
        // initialize_singleton();
        // if (block) {expression} else {expression2}
        let mut if_else = If::new(
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
        let mut tracker = IdentTracker::new();
        tracker.register(
            "block",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        tracker.register(
            "expression",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        tracker.register(
            "expression2",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        let test_stream = generate_if(&mut if_else, &mut tracker).unwrap();
        assert_eq!(
            "if block { expression } else { expression2 }",
            test_stream.to_string()
        );
    }

    #[test]
    fn test_if_elif_else() {
        // initialize_singleton();
        // if(block){expression} else if (block2){expression2} else {expression3}
        let mut if_elif_else = If::new(
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
        let mut tracker = IdentTracker::new();
        tracker.register(
            "block",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        tracker.register(
            "block2",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        tracker.register(
            "expression",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        tracker.register(
            "expression2",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        tracker.register(
            "expression3",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        let test_stream = generate_if(&mut if_elif_else, &mut tracker).unwrap();
        assert_eq!(
            "if block { expression } else if block2 { expression2 } else { expression3 }",
            test_stream.to_string()
        );
    }

    // for test
    #[test]
    fn test_simple_for_generation() {
        // for (i) in generator {hello}
        let mut simple_for = For::new(
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

        let mut tracker = IdentTracker::new();
        tracker.register("i", Identifier::new(IdentType::Other, TypeHint::Unknown));
        tracker.register(
            "generator",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        tracker.register(
            "hello",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        let test_stream = generate_for(&mut simple_for, &mut tracker).unwrap();
        assert_eq!("for i in generator { hello }", test_stream.to_string());
    }

    #[test]
    fn test_multi_arg_for_generation() {
        // for (a, b, c) in generator{hello}
        let mut multi_for = For::new(
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
        let mut tracker = IdentTracker::new();
        tracker.register("a", Identifier::new(IdentType::Other, TypeHint::Unknown));
        tracker.register("b", Identifier::new(IdentType::Other, TypeHint::Unknown));
        tracker.register("c", Identifier::new(IdentType::Other, TypeHint::Unknown));
        tracker.register(
            "generator",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        tracker.register(
            "hello",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        let test_stream = generate_for(&mut multi_for, &mut tracker).unwrap();
        assert_eq!(
            "for (a , b , c) in generator { hello }",
            test_stream.to_string()
        );
    }
    // While test
    #[test]
    fn test_simple_while() {
        let mut simple_while = While::new(
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "count",
                None,
                IdentType::Other,
            ))))),
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("expression", None, IdentType::Other)),
            ))))),
        );
        let mut tracker = IdentTracker::new();
        tracker.register(
            "count",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        tracker.register(
            "expression",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        let test_stream = generate_while(&mut simple_while, &mut tracker).unwrap();
        assert_eq!("while count { expression }", test_stream.to_string());
    }

    // FnDef test
    #[test]
    fn test_simple_fn_def() {
        // fn(block:i32, hello:str){expression}
        let mut simple_fn_def = FnDef::new(
            vec![
                Ident::new("block", Some(TypeDef::Integer32), IdentType::Other),
                Ident::new("hello", Some(TypeDef::Str), IdentType::Other),
            ],
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("expression", None, IdentType::Other)),
            ))))),
        );
        let mut tracker = IdentTracker::new();
        tracker.register(
            "block",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        tracker.register(
            "hello",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        tracker.register(
            "expression",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        let test_stream = generate_fn_def(&mut simple_fn_def, &mut tracker).unwrap();
        assert_eq!(
            "pub fn (block : i32 , hello : String) { expression }",
            test_stream.to_string()
        );
    }

    // FnCall test
    #[test]
    fn test_simple_fn_call() {
        // range()
        let mut simple_fn_call = FnCall::new(Ident::new("range", None, IdentType::Other), vec![]);
        let mut tracker = IdentTracker::new();
        tracker.register(
            "range",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        let test_stream = generate_fn_call(&mut simple_fn_call, None, &mut tracker, false).unwrap();
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
        let mut tracker = IdentTracker::new();
        tracker.register("Test", Identifier::new(IdentType::Other, TypeHint::Unknown));
        tracker.register("flag", Identifier::new(IdentType::Other, TypeHint::Unknown));
        let test_stream = generate_struct(&simple_struct, &mut tracker).unwrap();
        assert_eq!("struct Test { flag : bool }", test_stream.to_string());
    }

    // Return test
    #[test]
    fn test_simple_return() {
        // return hello
        let mut simple_return = Return::new(Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(
            Ident::new("hello", None, IdentType::Other),
        )))));
        let mut tracker = IdentTracker::new();
        tracker.register(
            "hello",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        let test_stream = generate_return(&mut simple_return, &mut tracker).unwrap();
        assert_eq!("return hello ;", test_stream.to_string());
    }

    // Comp expr test
    #[test]
    fn test_simple_comp() {
        // count > prev_count
        let mut comp_expr = Comp::new(
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
        let mut tracker = IdentTracker::new();
        tracker.register(
            "count",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        tracker.register(
            "prev_count",
            Identifier::new(IdentType::Other, TypeHint::Unknown),
        );
        let test_stream = generate_comp(&mut comp_expr, None, &mut tracker).unwrap();
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
        let test_stream = generate_array(&array_expr, &mut IdentTracker::new()).unwrap();
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
