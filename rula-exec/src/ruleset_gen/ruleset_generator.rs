use super::conf_parser::parse_config;
use super::error::RuleSetGenError;
use super::factory::generate_factory;
use super::tracker::*;
use super::types::*;
use super::IResult;
use rula_parser::parser::ast::*;

use std::cell::RefCell;

use std::path::PathBuf;

use proc_macro2::{Span, TokenStream};
use syn::Lit as SynLit;
use syn::{LitFloat, LitInt, LitStr};

pub type Scope = String;
pub type ValueTracker = RefCell<Tracker>;

/// Generate corresponding rust code from ast
/// Every nested generators returns a piece of TokenStream
/// Arguments:
///     ast_tree
pub fn generate(ast_tree: &AstNode, config_path: PathBuf) -> IResult<TokenStream> {
    // RuLa should know how many repeaters inconfig at this moment

    // Initialize tracker to track global state over the functions
    let tracker = RefCell::new(Tracker::new());

    // Parse config to check the number of repeaters
    let num_node = parse_config(&config_path).unwrap().len();
    let conf_path = String::from(config_path.to_str().unwrap());
    tracker.borrow_mut().update_num_node(num_node as u32);

    let ruleset_factory = generate_factory();

    // Generated rula program
    let tokens = match ast_tree {
        // All RuLa AST starts RuLa Node
        AstNode::RuLa(rula) => {
            // Go through entire AST nodes
            let generated = generate_rula(rula, &tracker).unwrap();

            // Names of rules
            let mut rule_names = vec![];
            let mut rule_initializers = vec![];
            for name in &tracker.borrow().rule_names {
                let rname = format_ident!("__{}", name);
                rule_names.push(rname.clone());
                rule_initializers.push(quote!(
                    let __rule = rula::#rname::new(Rc::clone(&ruleset_factory));
                    ruleset_factory.borrow_mut().add_rule_generator(#name, rula::RuleGenerators::#rname(__rule));
                ))
            }
            quote!(
                use rula_lib as rula_std;
                use rula_exec::ruleset_gen::ruleset::*;
                use rula_exec::ruleset_gen::condition::*;
                use rula_exec::ruleset_gen::action::*;
                use rula_exec::ruleset_gen::conf_parser;
                use rula_exec::ruleset_gen::types::*;


                use std::rc::Rc;
                use std::cell::RefCell;
                use std::fs::File;
                use std::io::Write;
                use itertools::Itertools;
                use rula_std::prelude::*;
                use std::collections::HashMap;
                #[allow(unused)]
                mod rula{
                    use super::*;

                    type RepeaterNumber = usize;
                    type RuleVec = Rc<RefCell<Vec<RefCell<Rule>>>>;
                    type FactoryType = Rc<RefCell<RuleSetFactory>>;
                    type RuleName = String;
                    use rula_exec::ruleset_gen::types::Repeater;

                    #[derive(Debug, Clone, PartialEq)]
                    pub struct RuleArgs{
                        pub args: HashMap<String, RuLaValue>
                    }

                    impl RuleArgs{
                        pub fn new() -> Self{
                            RuleArgs{
                                args: HashMap::new()
                            }
                        }
                        pub fn get(&self, arg_name: &str) -> &RuLaValue{
                            self.args.get(arg_name).expect("Failed to find the argument")
                        }
                        pub fn set(&mut self, arg_name: &str, value: RuLaValue){
                            self.args.insert(arg_name.to_string(), value);
                        }
                    }
                    #[derive(Debug, Clone, PartialEq)]
                    pub enum RuleGenerators{
                        #(#rule_names(#rule_names)),*
                    }

                    impl RuleGenerators {
                        pub fn generate(&mut self, _repeater: &Repeater, _arguments: &RuleArgs) -> Stage{
                            match self{
                                #(RuleGenerators::#rule_names(_rule) => {_rule.gen_rules(_repeater, _arguments)}),*
                            }
                        }
                        pub fn get_rule_args(&self) -> Vec<String>{
                            match self{
                                #(RuleGenerators::#rule_names(_rule) => {_rule.get_rule_args()}),*
                            }
                        }
                    }
                    // RuleSet Factory structure to store all the information to create RuleSets
                    #ruleset_factory

                    // Generated Rust programs
                    #generated
                }

                // cargo test --package tests-rulesetgen --test ruleset_generator
                // Anker function to start generating ruleset
                pub fn generate_ruleset(){
                    // parse config here
                    let repeaters = conf_parser::parse_config(&#conf_path.into()).unwrap();
                    // Register rules
                    let ruleset_factory = Rc::new(RefCell::new(rula::RuleSetFactory::from(repeaters)));
                    #(#rule_initializers)*

                    // generate_ruleset() in rula module
                    let rulesets = rula::generate_ruleset(Rc::clone(&ruleset_factory));

                    // <Repeater index, RuleSet>
                    for (i, ruleset) in rulesets.iter().enumerate(){
                        // Output file path (TODO: Should be flexible)
                        let output_file_path = format!("tests/generated/test_{}.json", i);
                        let mut file = File::create(output_file_path).expect("Failed to create a new file");
                        let json_ruleset = serde_json::to_string(ruleset).unwrap();
                        write!(&file, "{}", json_ruleset).unwrap();
                        file.flush().expect("Failed to write");
                        println!("{}", json_ruleset);
                    }
                }

                fn main(){
                    generate_ruleset()
                }

                #[cfg(test)]
                mod tests{
                    use super::*;
                    #[test]
                    fn test_rule_generate(){
                        generate_ruleset();
                        assert_eq!(1, 2);
                    }
                }

            )
        }

        // If there is a place holder, the value is not properly set
        AstNode::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    };

    // Run command to generate the rulesets
    Ok(tokens)
}

// Generate entire rula program
pub(super) fn generate_rula(rula: &RuLa, tracker: &ValueTracker) -> IResult<TokenStream> {
    match &*rula.rula {
        RuLaKind::Program(program) => Ok(generate_program(program, tracker).unwrap()),
        RuLaKind::Ignore => Ok(quote!()),
        RuLaKind::Eoi => Ok(quote!()),
        RuLaKind::PlaceHolder => Err(RuleSetGenError::InitializationError),
    }
}

// program: Program AST that contains a vector of Stmt
// program could have multiple expressions and statements inside
// program = { repeaters? ~ ( import_expr | rule_expr | ruleset_expr )* }
pub(super) fn generate_program(program: &Program, tracker: &ValueTracker) -> IResult<TokenStream> {
    // A vector to store a set of generated stmts
    let programs = {
        let mut programs = vec![];
        for program_kind in &program.programs {
            // Right now, a program can takes a stmt
            match program_kind {
                ProgramKind::Repeaters => {
                    // Prepare annotation
                }
                ProgramKind::Import(import_expr) => {
                    programs.push(generate_import(import_expr, tracker).unwrap());
                }
                ProgramKind::RuleSetExpr(ruleset_expr) => {
                    programs.push(generate_ruleset_expr(ruleset_expr, tracker).unwrap());
                }
                ProgramKind::RuleExpr(rule_expr) => {
                    programs.push(generate_rule_expr(rule_expr, tracker).unwrap());
                }
            }
        }
        programs
    };
    Ok(quote!(#(#programs)*))
}

// Import expression to get functions. (import test::func)
// Generate import expression
// import_expr = { ^"import" ~ ident ~ ( "::" ~ ident )* ~ ( "::" ~ "{" ~ ident_list ~ "}")?  ~ !"::" }
pub(super) fn generate_import(
    import_expr: &Import,
    tracker: &ValueTracker,
) -> IResult<TokenStream> {
    // import must be out of scope
    let scope = String::from("___*");

    // Convert a set of paths into a set of identifiers
    let path = import_expr.path.convert_to_ident();

    // Vector to store all paths
    // e.g. [hello::world, good::evening::Yall]
    let mut paths = vec![];
    for path_ident in path.iter() {
        let mut single_path = vec![];
        let quoted_path = if path_ident.name.contains("/") {
            let splitted = path_ident.name.split("/");
            for sp in splitted.into_iter() {
                let top_path = if sp == "std" { "rula_std" } else { sp };
                let new_ident = Ident::new(top_path, None);
                let path_fragment = generate_ident(&new_ident, tracker, &scope).unwrap();
                single_path.push(path_fragment)
            }
            let path_head = &single_path[0];
            let path_left = &single_path[1..];
            quote!(
                #path_head #(::#path_left)*
            )
        } else {
            let path_ident = generate_ident(path_ident, tracker, &scope).unwrap();
            quote!(#path_ident)
        };
        paths.push(quoted_path);
    }
    Ok(quote!(
        #(use #paths; )*
    ))
}

// Generate ruleset expression
// ruleset_expr = {^"ruleset" ~ ident ~ "{" ~ (stmt)* ~ "}" }
pub(super) fn generate_ruleset_expr(
    ruleset_expr: &RuleSetExpr,
    tracker: &ValueTracker,
) -> IResult<TokenStream> {
    // 0. Get the ruleset name and update the ruleset names
    let ruleset_name = &*ruleset_expr.name.name;
    let scope = ruleset_name;
    if tracker.borrow().check_rule_name_exist(ruleset_name) {
        return Err(RuleSetGenError::SameNameExistInRuleError);
    }

    // 1. Generate statements (stmt)* outside ruledef (in_ruledef = false)
    let mut generated = vec![];
    for stmt in &ruleset_expr.rules {
        generated.push(generate_stmt(stmt, tracker, scope, false, false).unwrap());
    }

    // RuleSet generate function
    let ruleset_generator = quote!(
        pub fn generate_ruleset(__factory: Rc<RefCell<RuleSetFactory>>) -> Vec<RuleSet> {
            // order corresponds to repeater number
            __factory.borrow().update_ruleset_name(#ruleset_name);
            #(#generated)*
            let mut rulesets = vec![];
            for rs in __factory.borrow().rulesets.borrow().iter(){
                rulesets.push(rs.borrow().clone());
            }
            rulesets
        }
    );

    Ok(ruleset_generator)
}

// Generate rule expression
// rule_expr = { ^"rule" ~ ident ~ "<" ~ repeater_ident ~ ">"~ argument_def
//              ~ (":->" ~ ret_type_annotation)? ~ "{" ~ rule_contents ~ "}" }
pub(super) fn generate_rule_expr(
    rule_expr: &RuleExpr,
    tracker: &ValueTracker,
) -> IResult<TokenStream> {
    // 0. (RuleName: ident) Get the rule name and register it to the tracker
    let rule_name = &*rule_expr.name.name;
    // Check if there is a Rule with the same name
    if tracker.borrow().check_rule_name_exist(rule_name) {
        return Err(RuleSetGenError::RuleNameDuplicationError);
    }
    let scope = rule_name;
    tracker.borrow_mut().register_rule_name(rule_name);
    let rule_name_structure_name = format_ident!("__{}", rule_name);

    // 1. (RepeaterIdent: ident) Register repeater identifiers to be used in this RuleExpr
    // e.g. <#rep> This can be refered as repeater value
    // Now the number of repeater is limited to 1
    let repeater_identifier = &*rule_expr.repeater_ident.name;
    tracker
        .borrow_mut()
        .add_internal_repeater_name(rule_name, repeater_identifier);

    // 2. Register given arguments to be used in this RuleExpr
    // e.g. (qubit1: qubit, result: RuLaResult)
    let mut arg_names = vec![];
    let argument_identifiers = {
        let mut argument_names = vec![];
        for arg in &*rule_expr.args {
            let arg_name = arg.name.to_string();
            arg_names.push(quote!(#arg_name.to_string()));
            match &*arg.type_hint {
                Some(hint) => {
                    let (type_hint, _) = generate_type_hint(hint).unwrap();
                    argument_names.push((arg_name, type_hint));
                }
                None => return Err(RuleSetGenError::NoTypeAnnotationError),
            }
        }
        argument_names
    };
    tracker
        .borrow_mut()
        .add_rule_arguments(rule_name, argument_identifiers);

    // 3. Check if there is a type annotation for return value or not
    let mut ret_type_annotation = RetTypeAnnotation::new(scope);
    match &*rule_expr.ret_type_annotation {
        Some(type_annoation) => {
            for (type_val, maybe) in &type_annoation.typedefs {
                let (type_info, _) = generate_type_hint(&type_val).unwrap();
                ret_type_annotation.add_return_type(type_info, *maybe);
            }
        }
        None => {}
    }
    tracker
        .borrow_mut()
        .add_return_type_annotation(rule_name, ret_type_annotation);

    // 4. Move into the RuleContent
    let generated_rule_contents =
        generate_rule_content(&rule_expr.rule_content, tracker, scope).unwrap();

    Ok(quote!(
        #[derive(Debug, Clone, PartialEq)]
        pub struct #rule_name_structure_name {
            // Order is the same as the def
            pub name: String,
            pub rule_args: Vec<String>,
            pub callback: Rc<RefCell<RuleSetFactory>>
        }

        impl #rule_name_structure_name {
            pub fn new(ruleset_factory: Rc<RefCell<RuleSetFactory>>) -> Self{
                #rule_name_structure_name{
                    name: #rule_name.to_string(),
                    rule_args: vec![#(#arg_names),*],
                    callback: ruleset_factory
                }
            }

            pub fn gen_rules(&mut self, repeater: &Repeater, arguments: &RuleArgs)-> Stage{
                let initial_rules = Rc::new(RefCell::new(vec![RefCell::new(Rule::new(#rule_name))]));
                let generated = self._generate(Rc::clone(&initial_rules), repeater, arguments);
                let mut stage = Stage::new();
                for rule in generated.borrow().iter(){
                    stage.add_rule(rule.borrow().clone())
                }
                stage
            }

            pub fn get_rule_args(&self) -> Vec<String>{
                self.rule_args.clone()
            }

            fn _generate(&mut self, rules: RuleVec, __repeater: &Repeater, __argument: &RuleArgs) -> RuleVec {
                // the key is the index of the repeater
                let __partner_rules: Rc<RefCell<HashMap<u64, RuleVec>>> = Rc::new(RefCell::new(HashMap::new()));

                // Generated rule contents
                #generated_rule_contents

                // add partner rules to the rulesets
                for (partner_index, rules) in __partner_rules.borrow().iter(){
                    let mut stage = Stage::new();
                    for rule in rules.borrow().iter(){
                        stage.add_rule(rule.borrow().clone());
                    }
                    self.callback.borrow().rulesets.borrow()[partner_index.clone() as usize].borrow_mut().add_stage(stage);
                }
                rules
            }
        }
    ))
}

// Generate contents of rule
// rule_contents = { ( let_stmt )* ~ cond_expr ~ "=>" ~ act_expr ~ ( stmt )* }
pub(super) fn generate_rule_content(
    rule_content_expr: &RuleContentExpr,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<TokenStream> {
    // Start rule definition
    let in_ruledef = true;

    // 0. Pre processing (local variable assignment)
    // Variables should be registered in this process
    let mut pre_processing = vec![];
    for let_stmt in rule_content_expr.pre_processing.iter() {
        // register local variables with scope
        pre_processing.push(generate_let(let_stmt, tracker, scope, in_ruledef).unwrap());
    }

    // 1. condition clause
    let condition_generator =
        generate_cond(&rule_content_expr.condition_expr, tracker, scope, true).unwrap();

    // 2. action clause
    let act_generator =
        generate_act(&*rule_content_expr.action_expr, tracker, scope, true).unwrap();

    // 3. Post processing
    let mut post_processing = vec![];
    for stmt in rule_content_expr.post_processing.iter() {
        post_processing.push(generate_stmt(stmt, tracker, scope, in_ruledef, false).unwrap());
    }

    Ok(quote!(
        #(#pre_processing)*
        #condition_generator
        #act_generator
        #(#post_processing)*
    ))
}

// Generate condition
// cond_expr = {^"cond" ~ "{"~ (cond_clauses)*  ~ "}"}
pub(super) fn generate_cond(
    cond_expr: &CondExpr,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    if !in_ruledef {
        panic!("Currently, the condition clause must be defined inside Rules")
    }

    // Get condition name
    let cond_name = match &*cond_expr.name {
        Some(name) => {
            let con_name = &*name.name;
            quote!(#con_name)
        }
        None => {
            quote!(None)
        }
    };

    // Generate condition clauses
    let mut clauses = vec![];
    for clause in cond_expr.clauses.iter() {
        let generated = generate_clauses(clause, tracker, scope, in_ruledef).unwrap();
        clauses.push(quote!(
            #generated;
        ))
    }
    Ok(quote!(
        let condition = Condition::new(#cond_name);
        // Add this clause to all the rules
        for rule in rules.borrow().iter(){
            rule.borrow_mut().set_condition(condition.clone());
        }
        #(#clauses)*
    ))
}

// Generate condition clauses
// cond_clauses = _{ res_assign | fn_call_expr | variable_call_expr }
pub(super) fn generate_clauses(
    cond: &CondClauses,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    let generated = match cond {
        CondClauses::ResAssign(res_assign) => {
            generate_res_assign(res_assign, tracker, scope, in_ruledef).unwrap()
        }
        CondClauses::FnCall(fn_call) => {
            generate_fn_call(fn_call, tracker, scope, in_ruledef).unwrap()
        }
        CondClauses::VariableCallExpr(var_call) => {
            generate_variable_call(var_call, tracker, scope, in_ruledef).unwrap()
        }
        CondClauses::Comp(comp_expr) => {
            generate_comp(comp_expr, tracker, scope, in_ruledef, false, false).unwrap()
        }
    };
    Ok(quote!(#generated))
}

// Resource Assignment for later use
// res_assign = {"@" ~ ident ~ ":" ~ fn_call_expr}
pub(super) fn generate_res_assign(
    res_assign: &ResAssign,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    // Generated return statement
    let name = generate_ident(&*res_assign.res_name, tracker, scope).unwrap();
    let fn_call = generate_fn_call(&*res_assign.fn_call, tracker, scope, in_ruledef).unwrap();
    Ok(quote!(let #name = #fn_call))
}

// Generate action clauses
// act_expr = { ^"act" ~ "{" ~ (stmt)* ~ "}"}
pub(super) fn generate_act(
    act_expr: &ActExpr,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    if !in_ruledef {
        panic!("Action clauses must be defined inside the Rule")
    }
    // Generated action clauses
    let mut generated = vec![];
    for stmt in act_expr.operatable.iter() {
        generated.push(generate_stmt(stmt, tracker, scope, in_ruledef, false).unwrap())
    }
    Ok(quote!(
        #(#generated)*
    ))
}

// Generate stmt expression
// Arguments:
//  stmt: Stmt AST node
//  rule_name: Option<String> a name of corresponding Rule
//  ident_tracker: List of identifiers that need to be tracked
//  stmt = { let_stmt | expr }
pub(super) fn generate_stmt(
    stmt: &Stmt,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
    in_match: bool,
) -> IResult<TokenStream> {
    // Statement can be let statement or series of expressions
    let statement = match &*stmt.kind {
        StmtKind::Let(let_stmt) => generate_let(let_stmt, tracker, scope, in_ruledef).unwrap(),
        StmtKind::Expr(expr) => generate_expr(expr, tracker, scope, in_ruledef, in_match).unwrap(),
        StmtKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    };
    Ok(quote!(#statement;))
}

/// Create a new variable that can be used inside the rule
/// Let statement doesn't affect rules. Just update local trackers
/// let_stmt = { ^"let" ~ ( ident_typed | "(" ~ ident_typed ~ ("," ~ ident_typed)* ~ ")" ) ~ "=" ~ expr}
pub(super) fn generate_let(
    let_stmt: &Let,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    // Register a variable to the tracker within the ruleset or rule
    if let_stmt.ident.len() == 1 {
        let identifier = &let_stmt.ident[0];
        match &*identifier.type_hint {
            Some(hint) => {
                let variable_name = format_ident!("{}", &*identifier.name);
                let (type_val, type_hint) = generate_type_hint(hint).unwrap();
                tracker
                    .borrow_mut()
                    .register_variable(&*identifier.name, &type_val, scope);
                let value =
                    generate_expr(&*let_stmt.expr, tracker, scope, in_ruledef, in_ruledef).unwrap();
                Ok(quote!(
                    let #variable_name: #type_hint = #value;
                ))
            }
            None => return Err(RuleSetGenError::NoTypeAnnotationError),
        }
    } else {
        let mut values = vec![];
        let mut type_annotations = vec![];
        // In mutlti
        for identifier in &let_stmt.ident {
            match &*identifier.type_hint {
                Some(hint) => {
                    let variable_name = format_ident!("{}", &*identifier.name);
                    let (type_val, type_hint) = generate_type_hint(hint).unwrap();
                    tracker
                        .borrow_mut()
                        .register_variable(&*identifier.name, &type_val, scope);
                    values.push(variable_name);
                    type_annotations.push(type_hint);
                }
                None => return Err(RuleSetGenError::NoTypeAnnotationError),
            }
        }
        let expression =
            generate_expr(&*let_stmt.expr, tracker, scope, in_ruledef, in_ruledef).unwrap();
        Ok(quote!(
            let (#(#values),*): (#(#type_annotations),*) = #expression;
        ))
    }
}

// expr = {
//     if_expr |
//     for_expr|
//     match_expr |
//     return_expr |
//     // non keyword expression
//     rule_call_expr|
//     send_expr |
//     // precedence sensitive
//     comp_expr |
//     term_expr |
//     vector |
//     tuple |
//     fn_call_expr |
//     variable_call_expr|
//     literal_expr
//   }
pub(super) fn generate_expr(
    expr: &Expr,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
    in_match: bool,
) -> IResult<TokenStream> {
    match &*expr.kind {
        ExprKind::If(if_expr) => Ok(generate_if(if_expr, tracker, scope, in_ruledef).unwrap()),
        ExprKind::For(for_expr) => Ok(generate_for(for_expr, tracker, scope, in_ruledef).unwrap()),
        ExprKind::Match(match_expr) => {
            Ok(generate_match(match_expr, tracker, scope, in_ruledef).unwrap())
        }
        ExprKind::Promote(promote_expr) => {
            Ok(generate_promote(promote_expr, tracker, scope, in_ruledef, in_match).unwrap())
        }
        ExprKind::RuleCall(rule_call_expr) => {
            Ok(generate_rule_call(rule_call_expr, tracker, scope).unwrap())
        }
        ExprKind::Send(send_expr) => {
            Ok(generate_send(send_expr, tracker, scope, in_ruledef, in_match).unwrap())
        }
        ExprKind::Set(set_expr) => Ok(generate_set(set_expr, tracker, scope, in_ruledef).unwrap()),
        ExprKind::Get(get_expr) => {
            Ok(generate_get(get_expr, tracker, scope, in_ruledef, in_match).unwrap())
        }
        ExprKind::Comp(comp_expr) => {
            Ok(generate_comp(comp_expr, tracker, scope, in_ruledef, in_match, false).unwrap())
        }
        ExprKind::Term(term_expr) => {
            Ok(generate_term_expr(term_expr, tracker, scope, in_ruledef).unwrap())
        }
        ExprKind::RuLaVec(rula_vec_expr) => {
            Ok(generate_vec(rula_vec_expr, tracker, scope).unwrap())
        }
        ExprKind::RuLaTuple(rula_tuple_expr) => {
            Ok(generate_tuple(rula_tuple_expr, tracker, scope, in_ruledef, in_match).unwrap())
        }
        ExprKind::FnCall(fn_call_expr) => {
            Ok(generate_fn_call(fn_call_expr, tracker, scope, in_ruledef).unwrap())
        }
        ExprKind::VariableCallExpr(variable_call_expr) => {
            Ok(generate_variable_call(variable_call_expr, tracker, scope, in_ruledef).unwrap())
        }
        ExprKind::Lit(lit_expr) => Ok(generate_lit(&lit_expr, tracker, scope).unwrap()),
        ExprKind::PlaceHolder => Err(RuleSetGenError::InitializationError),
    }
}

// Generate if expression
// if_expr = { ^"if" ~ "(" ~ expr ~ ")" ~ "{" ~ (stmt)* ~ "}" ~ ( else_if_expr )* ~ else_expr? }
// else_if_expr = { ^"else" ~ ^"if" ~ "(" ~ expr ~ ")" ~ "{" ~ (stmt)* ~ "}" }
// else_expr = { ^"else" ~ "{" ~ (stmt)* ~ "}"}
pub(super) fn generate_if(
    if_expr: &If,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    // If this is not the rule definition, this becomes just an ordinary if expression
    if !in_ruledef {
        // Generate expression that returns boolean value
        let block = generate_if_block(&*if_expr.block, tracker, scope, false, false).unwrap();

        // Generate statements
        let mut stmts = vec![];
        for st in if_expr.stmts.iter() {
            stmts.push(generate_stmt(st, tracker, scope, in_ruledef, false).unwrap())
        }

        // Generate elif statements
        let mut elifs = vec![];
        for elif in &if_expr.elif {
            let generated = generate_if(elif, tracker, scope, in_ruledef).unwrap();
            elifs.push(quote!(
                else #generated
            ));
        }

        // Generate else statements
        let mut elses = vec![];
        for els in &*if_expr.els {
            elses.push(generate_stmt(els, tracker, scope, in_ruledef, false).unwrap())
        }
        let generated_elses = if elses.len() > 0 {
            quote!(
                else{
                    #(#elses)*
                }
            )
        } else {
            quote!()
        };

        Ok(quote!(
            if #block {
                #(#stmts)*
            }
            #(#elifs)*
            #generated_elses
        ))
    } else {
        // // Collect all the block and statement
        let mut blocks: Vec<IfBlock> = vec![]; // if block that returns boolean value
                                               // TODO: consider removing clone
        blocks.push(*if_expr.block.clone());

        // Collect all the statements
        let mut statements: Vec<Vec<Stmt>> = vec![];
        statements.push(if_expr.stmts.clone());

        for elif in if_expr.elif.iter() {
            blocks.push(*elif.block.clone());
            statements.push(elif.stmts.clone());
        }

        // Translate blocks into rust code (This will be the condition clauses)
        let mut generated_conditions = vec![];
        let mut generated_actions = vec![];

        for block in blocks.iter() {
            let generated_block =
                generate_if_block(block, tracker, scope, in_ruledef, false).unwrap();
            generated_conditions.push(quote!(
                Box::new(
                    |__new_rule: RuleVec|{
                        #generated_block
                        __new_rule
                    }
                )
            ));
        }
        for action in statements.iter() {
            // Since this will be defined the same as the match closure, in_match = true.
            let mut generated = vec![];
            for stmt in action {
                generated.push(generate_stmt(stmt, tracker, scope, in_ruledef, true).unwrap());
            }
            let closure = quote!(Box::new(
                |__new_rule: RuleVec, mut __new_partner_rules: HashMap<u64, RuleVec>| {
                    #(#generated)*
                    (__new_rule, __new_partner_rules)
                }
            ));
            generated_actions.push(closure)
        }
        // If there is a else expression, need counterpart condition for all the expressions
        if if_expr.els.len() > 0 {
            // If there is more than one statement in els, this needs to be treated
            // else condition must be the inverse of all the previous conditions
            let mut inverse_blocks = vec![];
            for block in blocks.iter() {
                inverse_blocks
                    .push(generate_if_block(block, tracker, scope, in_ruledef, true).unwrap());
            }
            // Final else quote
            generated_conditions.push(quote!(
                Box::new(
                    |__new_rule:RuleVec|{
                        #(#inverse_blocks)*
                        __new_rule
                    }
                )
            ));

            let mut generated_stmt = vec![];
            for els_stmt in if_expr.els.iter() {
                generated_stmt
                    .push(generate_stmt(els_stmt, tracker, scope, in_ruledef, true).unwrap())
            }
            generated_actions.push(quote!(
                Box::new(
                    |__new_rule: RuleVec, mut __new_partner_rules: HashMap<u64, RuleVec>|{
                        #(#generated_stmt)*
                        (__new_rule, __new_partner_rules)
                    }
                )
            ))
        }

        // Inside the rule definition, if expression is expanded to the multiple rules
        // This works similarly to the match expressions
        // Take the current snapshot for the rules
        Ok(quote!(
            // Get the current snapshot of the rules
            let cloned_rules = rules.clone();
            let cloned_partner_rules = __partner_rules.borrow().clone();

            let mut new_rule_vec = vec![];
            let mut new_partner_map: HashMap<u64, RuleVec> = HashMap::new();

            let __if_actions: Vec<Box<dyn Fn(RuleVec, HashMap<u64, RuleVec>) -> ((RuleVec, HashMap<u64, RuleVec>))>> = vec![
                #(#generated_actions),*
            ];

            let __if_conditions: Vec<Box<dyn Fn(RuleVec) -> (RuleVec)>> = vec![
                #(#generated_conditions),*
            ];

            for (__action_func, __condition_func) in __if_actions.iter().zip(__if_conditions.iter()){
                let (gen_rules) = __condition_func(cloned_rules.clone());
                let (generated_rules, generated_partner_rules) = __action_func(gen_rules.clone(), cloned_partner_rules.clone());
                for gen_rule in &*generated_rules.borrow(){
                    new_rule_vec.push(gen_rule.clone());
                }
                for (rep_id, rule_vec) in generated_partner_rules.into_iter(){
                    if __partner_rules.borrow().contains_key(&rep_id){
                        for generated_partner_rule in rule_vec.borrow().iter(){
                            if !__partner_rules.borrow().get(&rep_id).expect("Failed to get repeater rules").borrow().contains(generated_partner_rule){
                                __partner_rules.borrow().get(&rep_id).expect("Failed to get repeater rules").borrow_mut().push(generated_partner_rule.clone());
                            }
                        }
                    }else{
                        __partner_rules.borrow_mut().insert(rep_id, rule_vec);
                    }
                }
            }
            // flush current rules and add new rules
            let mut rules = Rc::new(RefCell::new(vec![]));
            for new_rule in new_rule_vec{
                rules.borrow_mut().push(new_rule);
            }
        ))
    }
}

pub(super) fn generate_if_block(
    if_block: &IfBlock,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
    inverse: bool,
) -> IResult<TokenStream> {
    let in_match = if in_ruledef { true } else { false };
    match if_block {
        IfBlock::Get(get_expr) => {
            Ok(generate_get(get_expr, tracker, scope, in_ruledef, true).unwrap())
        }
        IfBlock::Comp(comp_expr) => {
            Ok(generate_comp(comp_expr, tracker, scope, in_ruledef, in_match, inverse).unwrap())
        }
        IfBlock::Lit(literal_expr) => Ok(generate_lit(literal_expr, tracker, scope).unwrap()),
        IfBlock::PlaceHolder => Err(RuleSetGenError::InitializationError),
    }
}

// Generate for expression
// for_expr = { ^"for" ~ ( ident | for_multi_block ) ~ "in" ~ for_generator ~ "{" ~ (stmt)* ~ "}" }
pub(super) fn generate_for(
    for_expr: &For,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    // Temporary variable block (e.g. i, (j, k))
    let block = &for_expr.variables;

    // If the number of temporary variables is more than one, it is surrounded by parens (...)
    let generated_block = if block.len() > 1 {
        let mut generated = vec![];
        for iden in block {
            generated.push(generate_ident(&iden, tracker, scope).unwrap());
        }
        quote!(
            (#(#generated),*)
        )
    } else {
        // len == 1
        let generated = generate_ident(&block[0], tracker, scope).unwrap();
        quote!(#generated)
    };

    // Expressions come after 'in'~
    let generator_expr = match &*for_expr.generator {
        ForGenerator::Series(series) => {
            generate_series(series, tracker, scope, in_ruledef).unwrap()
        }
        ForGenerator::Expr(expr) => generate_expr(expr, tracker, scope, in_ruledef, false).unwrap(),
        ForGenerator::PlaceHolder => {
            return Err(RuleSetGenError::InitializationError);
        }
    };

    // Statements that are in {...}
    let mut generated_stmts = vec![];
    for st in for_expr.stmts.iter() {
        generated_stmts.push(generate_stmt(st, tracker, scope, in_ruledef, false).unwrap());
    }
    Ok(quote!(
        for #generated_block in #generator_expr{
            #(#generated_stmts)*
        }
    ))
}

pub(super) fn generate_series(
    series: &Series,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    let start_val = generate_int_lit(&*series.start).unwrap();
    let end_val = generate_expr(&*series.end, tracker, scope, in_ruledef, false).unwrap();
    Ok(quote!(#start_val..#end_val))
}
pub(super) fn generate_fn_call(
    fn_call_expr: &FnCall,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    // Before generating functions, check function table to check whether it's properly defined or not
    let func_name = generate_ident(&*fn_call_expr.func_name, tracker, scope).unwrap();
    let mut generated_arguments = vec![];
    for arg in &fn_call_expr.arguments {
        generated_arguments.push(generate_fn_call_arg(arg, tracker, scope, in_ruledef).unwrap());
    }
    if in_ruledef {
        Ok(quote!(#func_name (Rc::clone(&rules), #(&#generated_arguments),*)))
    } else {
        Ok(quote!(#func_name ( #(&#generated_arguments),*)))
    }
}

pub(super) fn generate_fn_call_arg(
    fn_call_arg: &FnCallArgs,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    match fn_call_arg {
        FnCallArgs::FnCall(fn_call) => {
            Ok(generate_fn_call(fn_call, tracker, scope, in_ruledef).unwrap())
        }
        FnCallArgs::VariableCall(var_call) => {
            Ok(generate_variable_call(var_call, tracker, scope, in_ruledef).unwrap())
        }
        FnCallArgs::Lit(literals) => Ok(generate_lit(literals, tracker, scope).unwrap()),
        FnCallArgs::Term(term_expr) => {
            Ok(generate_term_expr(term_expr, tracker, scope, in_ruledef).unwrap())
        }
    }
}

// Generate rule call expression
// rule_call_expr = { ident ~ "<" ~ repeater_call ~ ">" ~ "(" ~ fn_call_args? ~ ("," ~ fn_call_args)* ~ ")"}
pub(super) fn generate_rule_call(
    rule_call_expr: &RuleCall,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<TokenStream> {
    // 0. check the rule name if the definition exists
    // If the rule name cannot be found, this can be compile error
    let rule_name = &*rule_call_expr.rule_name.name;
    if !tracker.borrow().check_rule_name_exist(rule_name) {
        println!("rule_name: {}", rule_name);
        return Err(RuleSetGenError::NoRuleFoundError);
    }

    // At this point, repeater argument and ordinary argument should be resolved
    // Currently the number of repater for one rule is limited to 1
    let (_, repeater_index) =
        generate_rep_call_arg(&*rule_call_expr.repeater_arg, tracker, scope, false).unwrap();

    // 0. get argument identifiers
    let mut arguments = vec![];
    for arg in &rule_call_expr.argument {
        arguments.push(generate_fn_call_arg(arg, tracker, scope, false).unwrap());
    }
    // 1. get argument types to wrap with Types enum (type error should be caught at this moment)
    // Check (supposed) argument types
    let supposed_arguments = tracker.borrow().get_rule_arguments(rule_name).clone();

    // Check whether the number of arguments does match
    if arguments.len() != supposed_arguments.len() {
        return Err(RuleSetGenError::ArgumentNumberError);
    }

    // 2. register arguments to rules (rule_name, arguments)
    let mut argument_register = vec![];
    for (gen_arg, (_, type_val)) in arguments.iter().zip(supposed_arguments.iter()) {
        let generated_type = generate_type_wrapper(gen_arg, type_val).unwrap();
        argument_register.push(generated_type)
    }

    // Lookup the supposed return values
    let return_types = if tracker.borrow().exist_return_types(rule_name) {
        let returns = tracker
            .borrow()
            .check_return_type_annotations(rule_name)
            .clone();
        let mut return_values = vec![];
        let mut evaluators = vec![];
        let mut maybes = vec![];
        if returns.return_types.len() > 0 {
            for (i, (type_hint, maybe)) in returns.return_types.iter().enumerate() {
                return_values.push(format_ident!("__ret__value{}", i));
                evaluators.push(generate_evaluator(type_hint).unwrap());
                maybes.push(maybe);
            }
            if returns.return_types.len() == 1 {
                quote!(
                    let __return_val_vec = __ruleset_factory.borrow().promoted_values(#rule_name);
                    let #(#return_values),*: &RuLaValue = &__return_val_vec[0];
                    (#(#return_values.#evaluators.clone()),*)
                )
            } else {
                quote!(
                    let __return_val_vec = __ruleset_factory.borrow().promoted_values(#rule_name);
                    let (#(#return_values),*) = __return_val_vec.iter().collect_tuple().unwrap();
                    (#(#return_values.#evaluators.clone()),*)
                )
            }
        } else {
            quote!()
        }
    } else {
        quote!()
    };
    // Add rule to stage inside the RuleSet
    Ok(quote!(
        // register current argument
        (|__ruleset_factory: Rc<RefCell<RuleSetFactory>>|{
            __ruleset_factory.borrow().register_args(#rule_name, vec![#(#argument_register),*]);
            __ruleset_factory.borrow().resolve_args(#rule_name);
            // Register arguments
            __ruleset_factory.borrow().genenerate_stage(#rule_name, #repeater_index);
            // Check returned values
            #return_types
        })(Rc::clone(&__factory))
    ))
}

// Repeater call must be casted to usize to
pub(super) fn generate_rep_call_arg(
    repeater_call_arg: &RepeaterCallArg,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<(TokenStream, TokenStream)> {
    let arg = match repeater_call_arg {
        RepeaterCallArg::Term(term) => {
            generate_term_expr(term, tracker, scope, in_ruledef).unwrap()
        }
        RepeaterCallArg::Ident(ident) => (generate_ident(ident, tracker, scope).unwrap()),
        RepeaterCallArg::IntegerLit(int_lit) => (generate_int_lit(int_lit).unwrap()),
        RepeaterCallArg::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    };
    Ok((quote!(__factory.borrow().repeaters.at(#arg)), quote!(#arg)))
}

// Promote resources to the next stage
pub(super) fn generate_promote(
    promote_expr: &Promote,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
    in_match: bool,
) -> IResult<TokenStream> {
    if !in_ruledef {
        panic!("promotion must be in rule definition")
    } else if !tracker.borrow().check_rule_name_exist(scope) {
        panic!("No rule found {}", scope);
    }
    // get the expected return types
    if !tracker.borrow().exist_return_types(scope) {
        panic!("No type annotation found in rule {}", scope);
    }

    let return_types = tracker
        .borrow()
        .check_return_type_annotations(scope)
        .clone();
    let mut expressions = vec![];
    for (promotable, (type_def, _maybe)) in promote_expr
        .target
        .iter()
        .zip(return_types.return_types.iter())
    {
        let generated_expr =
            generate_promotables(promotable, tracker, scope, in_ruledef, in_match).unwrap();
        let type_wrapped_expr = generate_type_wrapper(&generated_expr, type_def).unwrap();
        if !in_match {
            expressions
            .push(quote!(self.callback.borrow().promote(Rc::clone(&rules), &self.name, #type_wrapped_expr);));
        } else {
            expressions
            .push(quote!(self.callback.borrow().promote(Rc::clone(&__new_rule), &self.name, #type_wrapped_expr);));
        }
    }
    Ok(quote!(#(#expressions)*))
}

pub(super) fn generate_promotables(
    promotables: &Promotables,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
    in_match: bool,
) -> IResult<TokenStream> {
    match promotables {
        Promotables::Comp(comp_expr) => {
            Ok(generate_comp(comp_expr, tracker, scope, in_ruledef, false, false).unwrap())
        }
        Promotables::Term(term_expr) => {
            Ok(generate_term_expr(term_expr, tracker, scope, in_ruledef).unwrap())
        }
        Promotables::RuLaVec(vec_expr) => Ok(generate_vec(vec_expr, tracker, scope).unwrap()),
        Promotables::RuLaTuple(tuple_expr) => {
            Ok(generate_tuple(tuple_expr, tracker, scope, in_ruledef, in_match).unwrap())
        }
        Promotables::VariableCall(var_call_expr) => {
            Ok(generate_variable_call(var_call_expr, tracker, scope, in_ruledef).unwrap())
        }
        Promotables::Lit(lit_expr) => Ok(generate_lit(lit_expr, tracker, scope).unwrap()),
    }
}

// Generate send expression
// send_expr = { fn_call_expr ~ "->" ~ expr }
pub(super) fn generate_send(
    send_expr: &Send,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
    in_match: bool,
) -> IResult<TokenStream> {
    if !in_ruledef {
        panic!("Send expression must be inside the rule definition")
    }
    let func = &*send_expr.fn_call;
    let message_type = match &*func.func_name.name as &str {
        "free" => {
            quote!(ProtoMessageType::Free)
        }
        "update" => {
            quote!(ProtoMessageType::Update)
        }
        "meas" => {
            quote!(ProtoMessageType::Meas)
        }
        "transfer" => {
            quote!(ProtoMessageType::Transfer)
        }
        _ => return Err(RuleSetGenError::UnSendableFunctionError),
    };

    // This suppose to be repeater
    let expr = generate_expr(&*send_expr.expr, tracker, scope, in_ruledef, false).unwrap();

    // In send expression, the wait condition is also expaned to the rules in one stage
    if in_match {
        Ok(quote!(
            // Check if there is a predefined r
            if !__new_partner_rules.contains_key(&#expr.index){
                __new_partner_rules.insert(#expr.index, Rc::new(RefCell::new(vec![RefCell::new(Rule::new("Wait"))])));
            }
            // Send message to #expr
            send(Rc::clone(&__temp_rules), &#expr, #message_type);
            // Add wait rule to the partner ruleset
            wait(Rc::clone(__new_partner_rules.get(&#expr.index).expect("Failed to get corresponding rule vector")), &#expr, #message_type);
        ))
    } else {
        Ok(quote!(
            if !__partner_rules.borrow().contains_key(&#expr.index){
                __partner_rules.borrow_mut().insert(#expr.index, Rc::new(RefCell::new(vec![RefCell::new(Rule::new("Wait"))])));
            }
            send(Rc::clone(&rules), &#expr, #message_type);
            // Add wait clause to the partner ruleset
            wait(Rc::clone(__partner_rules.borrow().get(&#expr.index).expect("Failed to get the partner rule vector")), &#expr, #message_type);
            // wait(&self.callback.borrow().rulesets.borrow()[#expr.index as usize], __repeater, #message_type);
        ))
    }
}

pub(super) fn generate_set(
    set_expr: &Set,
    tracker: &ValueTracker,
    scope: &Scope,
    _in_ruledef: bool,
) -> IResult<TokenStream> {
    let value = &*set_expr.value;

    // check type def
    let type_hint = if tracker.borrow().check_variable_exist(&*value.name, scope) {
        tracker
            .borrow()
            .get_variable_type_hint(&*value.name, scope)
            .clone()
    } else {
        return Err(RuleSetGenError::NeedIdentifierTypeAnnotationError);
    };

    let alias = match &*set_expr.alias {
        Some(alias_value) => {
            let set_name = &*alias_value.name;
            tracker
                .borrow_mut()
                .register_set_variable(set_name, &type_hint);
            quote!(#set_name)
        }
        None => {
            let set_name = &*value.name;
            tracker
                .borrow_mut()
                .register_set_variable(set_name, &type_hint);
            quote!(#set_name)
        }
    };

    let generated_value = generate_type_wrapper(
        &generate_ident(&*set_expr.value, tracker, scope).unwrap(),
        &type_hint,
    )
    .unwrap();

    Ok(quote!(self.callback.borrow().set(Rc::clone(&rules), #generated_value, #alias);))
}

pub(super) fn generate_get(
    get_expr: &Get,
    tracker: &ValueTracker,
    _scope: &Scope,
    _in_ruledef: bool,
    in_match: bool,
) -> IResult<TokenStream> {
    let value = &*get_expr.value.name;
    // Check the value exist
    let type_hint = tracker
        .borrow()
        .get_type_hint_of_set_variable(value)
        .clone();
    let evaluator = generate_evaluator(&type_hint).unwrap();
    if !in_match {
        Ok(quote!(self.callback.borrow().get(Rc::clone(&rules), #value).#evaluator;))
    } else {
        Ok(quote!(self.callback.borrow().get(Rc::clone(&__new_rule), #value).#evaluator.clone()))
    }
}

// Generate Match expression to achieve match action
// In the case of static generation, this expand Rules to the stages
// match_expr = { ^"match" ~ expr ~ "{" ~ ( match_arm ~  ",")* ~ (^"otherwise" ~ "=>" ~ match_action)? ~ "}"}
pub(super) fn generate_match(
    match_expr: &Match,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    if !in_ruledef {
        panic!("Currently, match expression can be used only inside Rule expression")
    }
    // Generate expression (`in_match = false` since this can be true in match actions)
    let expr = generate_expr(&*match_expr.expr, tracker, scope, in_ruledef, false).unwrap();

    // Generate match arms and otherwise actions
    // `otherwise` includes the counter conditions of all the match arms
    let mut otherwise_conditions = vec![];
    let mut match_actions = vec![];
    for arm in match_expr.match_arms.iter() {
        let (otherwise_cond, match_action) =
            generate_match_arm(arm, tracker, scope, in_ruledef).unwrap();
        otherwise_conditions.push(otherwise_cond);
        match_actions.push(match_action);
    }

    // The number of rules after the match expression can be calculated by (current rule number)*(number of match arms + 1 or 0)
    let mut final_rule_count = match_actions.len();

    let otherwise = match &*match_expr.otherwise {
        Some(other) => {
            let generated = generate_match_action(other, tracker, scope, in_ruledef).unwrap();
            // Add +1 to for other wise rule
            final_rule_count += 1;
            quote!(
                // Closure to get the vector for the rule and corresponding partner rules
                let (generated_vec, generated_partner_map) =
                (|__new_rule: RuleVec, mut __new_partner_rules: HashMap<u64, RuleVec>|{
                    let mut __t_rules = vec![];
                    for _temp_rule in __new_rule.borrow().iter(){
                        #(#otherwise_conditions)*
                        __t_rules.push(_temp_rule.clone());
                    }
                    let __temp_rules = Rc::new(RefCell::new(__t_rules));
                    #generated;
                    (__temp_rules, __new_partner_rules)
                })(cloned_rules.clone(), cloned_partner_rules.clone());

                // Push generated rules into rules
                for gen_rule in generated_vec.borrow().iter(){
                    rules.borrow_mut().push(gen_rule.clone())
                }

                for (rep_id, rule_vec) in generated_partner_map.into_iter(){
                    if __partner_rules.borrow().contains_key(&rep_id){
                        for generated_partner_rule in rule_vec.borrow().iter(){
                            if !__partner_rules.borrow().get(&rep_id).expect("Failed to get repeater rules").borrow().contains(generated_partner_rule){
                                __partner_rules.borrow().get(&rep_id).expect("Failed to get repeater rules").borrow_mut().push(generated_partner_rule.clone());
                            }else{
                                __partner_rules.borrow_mut().insert(rep_id, rule_vec.clone());
                            }
                        }
                    }
                }


            )
        }
        None => {
            quote!()
        }
    };
    // match does not mapped to match expression
    let count = SynLit::Int(LitInt::new(
        &final_rule_count.to_string(),
        Span::call_site(),
    ));
    Ok(quote!(
        let  __cmp_target = #expr.comparable();
        let current_rule_count = rules.borrow().len();
        let final_rule_count = current_rule_count * #count;

        let __match_actions: Vec<Box<dyn Fn(RuleVec, HashMap<u64, RuleVec>) -> ((RuleVec, HashMap<u64, RuleVec>))>> = vec![
            #(#match_actions),*
        ];

        let cloned_rules = rules.clone();
        let cloned_partner_rules = __partner_rules.borrow().clone();

        let mut new_rule_vec = vec![];
        let mut new_partner_map: HashMap<u64, RuleVec> = HashMap::new();
        for  __action_func in &__match_actions{
            let (generated_rules, generated_partner_rules) = __action_func(cloned_rules.clone(), cloned_partner_rules.clone());
            for gen_rule in &*generated_rules.borrow(){
                new_rule_vec.push(gen_rule.clone());
            }
            for (rep_id, rule_vec) in generated_partner_rules.into_iter(){
                if __partner_rules.borrow().contains_key(&rep_id){
                    for generated_partner_rule in rule_vec.borrow().iter(){
                        if !__partner_rules
                        .borrow()
                        .get(&rep_id)
                        .expect("Failed to get repeater rules")
                        .borrow()
                        .contains(generated_partner_rule){
                            __partner_rules.borrow()
                            .get(&rep_id)
                            .expect("Failed to get repeater rules")
                            .borrow_mut()
                            .push(generated_partner_rule.clone());
                        }
                    }
                }else{
                    __partner_rules.borrow_mut().insert(rep_id, rule_vec);
                }
            }

        }

        let mut rules = Rc::new(RefCell::new(vec![]));
        for new_rule in new_rule_vec{
            rules.borrow_mut().push(new_rule);
        }

        #otherwise

        let num_rules = rules.borrow().len();
        if  num_rules != final_rule_count {
            panic!("The final rule size is wrong Suppose: {} != Actual{}", final_rule_count, num_rules);
        }


    ))
}

// Generate match arms
// match_arm = { match_condition ~ "=>" ~ match_action }
pub(super) fn generate_match_arm(
    match_arm: &MatchArm,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<(TokenStream, TokenStream)> {
    let match_condition =
        generate_match_condition(&*match_arm.condition, tracker, scope, in_ruledef).unwrap();
    let match_actions =
        generate_match_action(&*match_arm.action, tracker, scope, in_ruledef).unwrap();
    Ok((
        quote!(
            _temp_rule.borrow_mut().add_condition_clause(
                ConditionClauses::Cmp(Cmp::new(CmpOp::Neq, __cmp_target(#match_condition)))
            );
        ),
        quote!(
            Box::new(|__new_rule, mut __new_partner_rules|{
                let mut __t_rules = vec![];
                for _temp_rule in __new_rule.borrow().iter(){
                    _temp_rule.borrow_mut().add_condition_clause(
                        ConditionClauses::Cmp(Cmp::new(CmpOp::Eq, __cmp_target(#match_condition)))
                    );
                    __t_rules.push(_temp_rule.clone());
                }
                let __temp_rules = Rc::new(RefCell::new(__t_rules));
                #match_actions;
                (__temp_rules, __new_partner_rules)
            })
        ),
    ))
}

// Right now, the match condition can only be literal expression
// match_condition = { satisfiable }
pub(super) fn generate_match_condition(
    match_condition: &MatchCondition,
    tracker: &ValueTracker,
    scope: &Scope,
    _in_ruledef: bool,
) -> IResult<TokenStream> {
    // Right now, satisfiable can only take literals, but in the future, this should be more flexible
    match &*match_condition.satisfiable {
        Satisfiable::Lit(literal) => Ok(generate_lit(literal, tracker, scope).unwrap()),
        _ => Err(RuleSetGenError::InitializationError),
    }
}

// Match actions that are the series of expressions
// match_action = { "{" ~ expr? ~ ("," ~ expr)* ~ "}" }
pub(super) fn generate_match_action(
    match_action: &MatchAction,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    let mut actionables = vec![];
    for actionable in &match_action.actionable {
        actionables.push(generate_expr(actionable, tracker, scope, in_ruledef, true).unwrap());
    }
    Ok(quote!(
        #(#actionables);*
    ))
}

// Generate comparison expression such as A == B
// comp_expr = { comparable ~ comp_op ~ comparable }
// comparable = { term_expr | variable_call_expr | fn_call_expr |  literal_expr }
// comp_op = { "<" | ">" | "=<" | ">=" | "==" | "!=" }
pub(super) fn generate_comp(
    comp_expr: &Comp,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
    in_match: bool,
    inverse: bool,
) -> IResult<TokenStream> {
    let lhs = generate_comparable(&comp_expr.lhs, tracker, scope, in_ruledef, in_match).unwrap();
    let rhs = generate_comparable(&comp_expr.rhs, tracker, scope, in_ruledef, in_match).unwrap();
    let (op, cmp_op) = match *comp_expr.comp_op {
        CompOpKind::Lt => {
            if !inverse {
                (quote!(<), quote!(CmpOp::Lt))
            } else {
                (quote!(<), quote!(CmpOp::Gt))
            }
        }
        CompOpKind::Gt => {
            if !inverse {
                (quote!(>), quote!(CmpOp::Gt))
            } else {
                (quote!(>), quote!(CmpOp::Lt))
            }
        }
        CompOpKind::LtE => {
            if !inverse {
                (quote!(<=), quote!(CmpOp::LtE))
            } else {
                (quote!(<=), quote!(CmpOp::GtE))
            }
        }
        CompOpKind::GtE => {
            if !inverse {
                (quote!(>=), quote!(CmpOp::GtE))
            } else {
                (quote!(>=), quote!(CmpOp::LtE))
            }
        }
        CompOpKind::Eq => {
            if !inverse {
                (quote!(==), quote!(CmpOp::Eq))
            } else {
                (quote!(==), quote!(CmpOp::Neq))
            }
        }
        CompOpKind::Nq => {
            if !inverse {
                (quote!(!=), quote!(CmpOp::Neq))
            } else {
                (quote!(!=), quote!(CmpOp::Eq))
            }
        }
        CompOpKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    };
    if !in_match {
        Ok(quote!(#lhs #op #rhs))
    } else {
        Ok(quote!(
            // Left value could be : Result, int,

            // If either __lhs_val_name or __rhs_val_name is not "_", then that is get expression
            let (__test_lhs, __lhs_val_name) = #lhs;
            let (__test_rhs, __rhs_val_name) = #rhs;

            if __lhs_val_name == "*" && __rhs_val_name == "*"{
                todo!("Currently, either left or right hand side value needs to be ruleset variable")
            }
            // TODO: ordinary if expression
            let (__cmp_target, compare_value) = if __lhs_val_name != "*"{
                // means lhs is get
                (__test_rhs.comparable(), __lhs_val_name)
            } else if __rhs_val_name != "*"{
                // means rhs is get
                (__test_lhs.comparable(), __rhs_val_name)
            }else{
                todo!("Currently, both value cannot be get in comparison")
            };

            for rule in __new_rule.borrow().iter(){
                rule.borrow_mut().add_condition_clause(ConditionClauses::Cmp(Cmp::new(#cmp_op, __cmp_target(compare_value))));
            }
        ))
    }
}

pub(super) fn generate_comparable(
    comparable: &Comparable,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
    in_match: bool,
) -> IResult<TokenStream> {
    match comparable {
        Comparable::Get(get_expr) => {
            let generated = generate_get(get_expr, tracker, scope, in_ruledef, in_match).unwrap();
            let target_value = &*get_expr.value.name;
            if in_match {
                Ok(quote!((#generated.clone(), #target_value)))
            } else {
                Ok(quote!(#generated))
            }
        }
        Comparable::Term(term_expr) => {
            let generated = generate_term_expr(term_expr, tracker, scope, in_ruledef).unwrap();
            if in_match {
                Ok(quote!((#generated, "*")))
            } else {
                Ok(quote!(#generated))
            }
        }
        Comparable::VariableCall(var_call_expr) => {
            let generated =
                generate_variable_call(var_call_expr, tracker, scope, in_ruledef).unwrap();
            if in_match {
                Ok(quote!((#generated.clone(), "*")))
            } else {
                Ok(quote!(#generated))
            }
        }
        Comparable::FnCall(fn_call_expr) => {
            let generated = generate_fn_call(fn_call_expr, tracker, scope, in_ruledef).unwrap();
            Ok(quote!((#generated.clone(), "*")))
        }
        Comparable::Lit(lit_expr) => {
            let generated = generate_lit(lit_expr, tracker, scope).unwrap();
            if in_match {
                Ok(quote!((#generated, "*")))
            } else {
                Ok(quote!(#generated))
            }
        }
        Comparable::PlaceHolder => Err(RuleSetGenError::InitializationError),
    }
}

// Generate Variable call expression (e.g. test.function())
// variable_call_expr = { callable ~ "." ~ callable ~ ("." ~ callable)*}
// callable = { fn_call_expr | repeater_ident | ident }
pub(super) fn generate_variable_call(
    variable_call_expr: &VariableCallExpr,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    let mut generated = vec![];

    for callable in &variable_call_expr.variables {
        match callable {
            Callable::FnCall(fn_call) => {
                generated.push(generate_fn_call(fn_call, tracker, scope, in_ruledef).unwrap());
            }
            Callable::Ident(ident) => {
                generated.push(generate_ident(ident, tracker, scope).unwrap())
            }
            Callable::RepeaterIdent(_) => {
                if in_ruledef {
                    generated.push(quote!(__repeater))
                } else {
                    generated.push(quote!(__factory.borrow().repeaters))
                }
            }
        }
    }
    Ok(quote!(#(#generated).*))
}

// Generate Vector expression vec![1, 2, 3, ...]
// vector = {"[" ~ literal_expr? ~ ("," ~ literal_expr)* ~ ","? ~ "]"}
pub(super) fn generate_vec(
    vector_expr: &RuLaVec,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<TokenStream> {
    let mut items = vec![];
    for lit in &vector_expr.items {
        items.push(generate_lit(lit, tracker, scope).unwrap());
    }
    Ok(quote!(vec![#(#items),*]))
}

// Generate tuple expression (e.g. (1, 2, 3..))
// tuple = {"(" ~ expr? ~ ("," ~ expr)* ~ ","? ~ ")"}
pub(super) fn generate_tuple(
    tuple_expr: &RuLaTuple,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
    in_match: bool,
) -> IResult<TokenStream> {
    let mut items = vec![];
    for exp in &tuple_expr.items {
        items.push(generate_expr(exp, tracker, scope, in_ruledef, in_match).unwrap());
    }
    Ok(quote!((#(#items),*)))
}

// Generate literal expressions
// literal_expr = { ( bool | string | ident | number | binary | hex | unicord ) }
pub(super) fn generate_lit(
    lit: &Lit,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<TokenStream> {
    match &*lit.kind {
        LitKind::Ident(ident_lit) => Ok(generate_ident(ident_lit, tracker, scope).unwrap()),
        LitKind::NumberLit(number_lit) => {
            Ok(generate_number_lit(number_lit, tracker, scope).unwrap())
        }
        LitKind::StringLit(string_lit) => {
            let val = SynLit::Str(LitStr::new(&string_lit.as_string(), Span::call_site()));
            Ok(quote!(#val))
        }
        LitKind::BooleanLit(boolean_lit) => {
            if *boolean_lit {
                Ok(quote!(true))
            } else {
                Ok(quote!(false))
            }
        }
        LitKind::HexLit(hex_lit) => {
            let raw_value = &*hex_lit.value;
            let without_prefix = raw_value.trim_start_matches("0x");
            Ok(quote!(i64::from_str_radix(#without_prefix, 16).unwrap()))
        }
        LitKind::BinaryLit(bin_lit) => {
            let raw_value = &*bin_lit.value;
            let without_prefix = raw_value.trim_start_matches("0b");
            Ok(quote!(i64::from_str_radix(#without_prefix, 2).unwrap()))
        }
        LitKind::UnicordLit(unicord) => {
            let value = &*unicord.value;
            Ok(quote!(String::from(#value)))
        }
        _ => todo!("{:#?} not yet implemented", lit),
    }
}

// Generate a number literal
// number = {  ( plus | minus )? ~ ( float | int | ident ) ~ ( "e" ~ ( "+" | "-" )? ~ ASCII_DIGIT+ )? }
pub(super) fn generate_number_lit(
    number_lit: &NumberLit,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<TokenStream> {
    match &number_lit.kind {
        NumberLitKind::IntegerLit(int_lit) => Ok(generate_int_lit(int_lit).unwrap()),
        NumberLitKind::FloatLit(float_lit) => Ok(generate_float_lit(float_lit).unwrap()),
        NumberLitKind::NumIdentLit(num_ident_lit) => {
            let value = generate_ident(&num_ident_lit.value, tracker, scope).unwrap();
            if num_ident_lit.negative {
                Ok(quote!(-#value))
            } else {
                Ok(quote!(#value))
            }
        }
        NumberLitKind::PlaceHolder => Err(RuleSetGenError::InitializationError),
    }
}

// Generate an integer literal
// int = @{ ( ASCII_NONZERO_DIGIT ~ ASCII_DIGIT+ | ASCII_DIGIT ) ~ !ASCII_ALPHA}
pub(super) fn generate_int_lit(integer_lit: &IntegerLit) -> IResult<TokenStream> {
    let value = SynLit::Int(LitInt::new(&integer_lit.value.name, Span::call_site()));
    if integer_lit.negative {
        Ok(quote!(-#value))
    } else {
        Ok(quote!(#value))
    }
}

// Generate a floating value
// float = {int ~ "." ~ int}
pub(super) fn generate_float_lit(float_lit: &FloatLit) -> IResult<TokenStream> {
    let value = SynLit::Float(LitFloat::new(&float_lit.value.name, Span::call_site()));
    if float_lit.negative {
        Ok(quote!(-#value))
    } else {
        Ok(quote!(#value))
    }
}

// Generate a term expression
// Currently, regardless of the order of precedences, the prenthesis needs to be added.
// (e.g. (test/2) + 1) to say the first term is (test/2)
// term_expr = { inner_term ~ ( op ~ inner_term )+ }
// inner_term = _{ terms | "(" ~ term_expr ~ ")" }
// terms = { variable_call_expr | fn_call_expr | literal_expr }
pub(super) fn generate_term_expr(
    term_expr: &Term,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    // We could make Term struct instead of direct calc
    // For now, this function just returns calc result as f64
    // let val = LitFloat::new(&term_expr.to_string(), Span::call_site());
    let lhs = generate_term_element(&term_expr.lhs, tracker, scope, in_ruledef).unwrap();
    let rhs = generate_term_element(&term_expr.rhs, tracker, scope, in_ruledef).unwrap();
    let operator = match &*term_expr.op {
        TermOps::Plus => {
            quote!(+)
        }
        TermOps::Minus => {
            quote!(-)
        }
        TermOps::Asterisk => {
            quote!(*)
        }
        TermOps::Slash => {
            quote!(/)
        }
        TermOps::Percent => {
            quote!(%)
        }
        TermOps::Caret => {
            quote!(^)
        }
        TermOps::PlaceHolder => {
            return Err(RuleSetGenError::InitializationError);
        }
    };
    Ok(quote!(#lhs #operator #rhs))
}

pub(super) fn generate_term_element(
    term: &Terms,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    match term {
        Terms::FnCall(fn_call_expr) => {
            Ok(generate_fn_call(fn_call_expr, tracker, scope, in_ruledef).unwrap())
        }
        Terms::VariableCallExpr(var_call_expr) => {
            Ok(generate_variable_call(var_call_expr, tracker, scope, in_ruledef).unwrap())
        }
        Terms::Lit(lit_expr) => Ok(generate_lit(lit_expr, tracker, scope).unwrap()),
        Terms::Term(term_expr) => {
            let generated_term = generate_term_expr(term_expr, tracker, scope, in_ruledef).unwrap();
            Ok(quote!((#generated_term)))
        }
        Terms::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    }
}

// Generate identifier token from Ident ast
// ident = @{ ASCII_ALPHA~ ( ASCII_ALPHA | ASCII_DIGIT | "_" )* }
// ident_list = { ( ident | ident_typed ) ~ ( "," ~ ( ident | ident_typed ) )* }
pub(super) fn generate_ident(
    ident: &Ident,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<TokenStream> {
    if &*ident == &Ident::place_holder() {
        return Err(RuleSetGenError::InitializationError);
    }

    // If this is an argument, get from argument list
    let (exist, type_def) = tracker.borrow().check_rule_arguments(scope, &*ident.name);
    if exist {
        let identifier_name = &*ident.name;
        let evaluator = generate_evaluator(&type_def).unwrap();
        return Ok(quote!(__argument.get(#identifier_name).#evaluator));
    }

    let identifier = format_ident!("{}", *ident.name);
    Ok(quote!(#identifier))
}

pub(super) fn generate_type_hint(type_hint: &TypeDef) -> IResult<(Types, TokenStream)> {
    match type_hint {
        TypeDef::Repeater => Ok((Types::Repeater, quote!(&Repeater))),
        TypeDef::Result => Ok((Types::Result, quote!(RuLaResult))),
        TypeDef::Boolean => Ok((Types::Boolean, quote!(bool))),
        TypeDef::Integer => Ok((Types::Int, quote!(i64))),
        TypeDef::UnsignedInteger => Ok((Types::UInt, quote!(u64))),
        TypeDef::Float => Ok((Types::Float, quote!(f64))),
        TypeDef::Str => Ok((Types::Str, quote!(String))),
        TypeDef::Qubit => Ok((Types::Qubit, quote!(Qubit))),
        TypeDef::Vector(inner) => {
            let (inner_type_def, inner_type) = generate_type_hint(inner).unwrap();
            Ok((
                Types::Vec(Box::new(inner_type_def)),
                quote!(Vec<#inner_type>),
            ))
        }
        TypeDef::PlaceHolder => return Err(RuleSetGenError::InitializationError),
        _ => todo!("type {:#?} not yet implemented", type_hint),
    }
}

pub(super) fn generate_evaluator(type_def: &Types) -> IResult<TokenStream> {
    match type_def {
        Types::Repeater => Ok(quote!(eval_as_repeater())),
        Types::Message => Ok(quote!(eval_as_message())),
        Types::Result => Ok(quote!(eval_as_result())),
        Types::Qubit => Ok(quote!(eval_as_qubit())),
        Types::Int => Ok(quote!(eval_as_int())),
        Types::UInt => Ok(quote!(eval_as_uint())),
        Types::Float => Ok(quote!(eval_as_float())),
        Types::Boolean => Ok(quote!(eval_as_bool())),
        Types::Str => Ok(quote!(eval_as_str())),
        Types::Vec(_) => Ok(quote!(eval_as_vec())),
        Types::Unknown => Err(RuleSetGenError::UnknownTypeError),
    }
}

pub(super) fn generate_type_wrapper(expr: &TokenStream, type_def: &Types) -> IResult<TokenStream> {
    match type_def {
        Types::Repeater => Ok(quote!(RuLaValue::Repeater(#expr.clone()))),
        Types::Message => Ok(quote!(RuLaValue::Message(#expr.clone()))),
        Types::Result => Ok(quote!(RuLaValue::RuLaResult(#expr.clone()))),
        Types::Qubit => Ok(quote!(RuLaValue::Qubit(#expr.clone()))),
        Types::Int => Ok(quote!(RuLaValue::Int(#expr as i64))),
        Types::UInt => Ok(quote!(RuLaValue::UInt(#expr as u64))),
        Types::Float => Ok(quote!(RuLaValue::Float(#expr as f64))),
        Types::Boolean => Ok(quote!(RuLaValue::Boolean(#expr as bool))),
        Types::Str => Ok(quote!(RuLaValue::Str(#expr))),
        Types::Vec(_) => Ok(quote!(RuLaValue::Vector(#expr))),
        Types::Unknown => Err(RuleSetGenError::UnknownTypeError),
    }
}
