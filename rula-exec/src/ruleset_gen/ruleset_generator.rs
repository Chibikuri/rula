use super::conf_parser::parse_config;
use super::error::RuleSetGenError;
use super::factory::generate_factory;
use super::ruleset::{Rule, RuleSet, Stage};
use super::tracker::{
    Arguments, LocalVariableTracker, RetTypeAnnotation, Tracker, VarKind, Variable,
};
use super::types::*;
use super::IResult;
use rula_parser::parser::ast::*;

use std::cell::RefCell;

use std::io::{BufReader, BufWriter, Write};
use std::path::PathBuf;
use std::process::Command;
use std::rc::Rc;

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

    let num_node = parse_config(config_path.clone()).unwrap().len();
    let conf_path = String::from(config_path.to_str().unwrap());
    tracker.borrow_mut().update_num_node(num_node as u32);
    // Add empty RuleSets
    for i in 0..num_node {
        tracker.borrow_mut().add_ruleset(i, RuleSet::new("empty"));
    }

    let ruleset_factory = generate_factory();
    // Generated rula program
    let tokens = match ast_tree {
        // All RuLa AST starts RuLa Node
        AstNode::RuLa(rula) => {
            let generated = generate_rula(rula, &tracker).unwrap();
            let rule_generators = {
                let mut rule_name_idents = vec![];
                for name in &tracker.borrow().rule_names {
                    let rname = format_ident!("__{}", name);
                    rule_name_idents.push(quote!(#rname (#rname)));
                }
                let mut rule_gen_idents = vec![];
                let mut arg_gen = vec![];
                for name in &tracker.borrow().rule_names {
                    let rname = format_ident!("__{}", name);
                    rule_gen_idents.push(quote!(
                        RuleGenerators::#rname(_rule) => {_rule.gen_rules(_repeater, _arguments)}
                    ));
                    arg_gen.push(quote!(
                        RuleGenerators::#rname(_rule) => {_rule.get_rule_args()}
                    ));
                }
                quote!(
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
                            #(#rule_name_idents),*
                        }

                        impl RuleGenerators{
                            pub fn generate(&mut self, _repeater: &Repeater, _arguments: &RuleArgs) -> Stage{
                                match self{
                                    #(#rule_gen_idents),*
                                }
                            }
                            pub fn get_rule_args(&self) -> Vec<String>{
                                match self{
                                    #(#arg_gen),*
                                }
                            }
                        }
                )
            };

            let mut rule_adder = vec![];
            for name in &tracker.borrow().rule_names {
                let rname = format_ident!("__{}", name);
                rule_adder.push(quote!(
                    let __rule = rula::#rname::new(Rc::clone(&ruleset_factory));
                    ruleset_factory.borrow_mut().add_rule_generator(#name, rula::RuleGenerators::#rname(__rule));
                ))
            }
            quote!(
                use rula_lib as rula_std;
                use rula_exec::ruleset_gen::ruleset::{RuleSet, Rule, Stage};
                use rula_exec::ruleset_gen::condition::*;
                use rula_exec::ruleset_gen::action::*;
                use rula_exec::ruleset_gen::conf_parser;
                use rula_exec::ruleset_gen::types::*;


                use std::rc::Rc;
                use std::cell::RefCell;
                use std::fs::File;
                use std::io::Write;
                use rula_std::prelude::*;
                use std::collections::HashMap;
                #[allow(unused)]
                mod rula{
                    use super::*;

                    type RepeaterNumber = usize;
                    type RuleVec = Rc<RefCell<Vec<RefCell<Rule>>>>;
                    type FactoryType = Rc<RefCell<RuleSetFactory>>;
                    use rula_exec::ruleset_gen::types::Repeater;

                    #rule_generators
                    #ruleset_factory
                    #generated
                }
                // cargo test --package tests-rulesetgen --test ruleset_generator
                pub fn generate_ruleset(){
                    // parse config here
                    let mut repeaters = conf_parser::parse_config(#conf_path.into()).unwrap();
                    for (i, rep) in repeaters.iter_mut().enumerate(){
                        rep.update_index(i as u64);
                    }
                    // Register rules
                    let ruleset_factory = Rc::new(RefCell::new(rula::RuleSetFactory::from(repeaters)));
                    #(#rule_adder)*
                    let rulesets = rula::generate_ruleset(Rc::clone(&ruleset_factory));
                    for (i, ruleset) in rulesets.iter().enumerate(){
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
        AstNode::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    };

    // generate_token_stream_file(tokens, "generate.rs");
    // Run command to generate the rulesets
    // let rulesets = tracker.borrow().return_rulesets();
    Ok(tokens)
}

// Generate entire rula program
pub(super) fn generate_rula(rula: &RuLa, tracker: &ValueTracker) -> IResult<TokenStream> {
    let program = match &*rula.rula {
        RuLaKind::Program(program) => generate_program(program, tracker).unwrap(),
        RuLaKind::Ignore => {
            quote!()
        }
        RuLaKind::Eoi => {
            quote!()
        }
        RuLaKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    };
    Ok(program)
}

// Generate progra
// program: Program AST that contains a vector of Stmt
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

pub(super) fn generate_import(
    import_expr: &Import,
    tracker: &ValueTracker,
) -> IResult<TokenStream> {
    // import must be out of scope
    let scope = String::from("");
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
    tracker.borrow_mut().update_ruleset_name(ruleset_name);

    let mut generated = vec![];
    for stmt in &ruleset_expr.rules {
        generated.push(generate_stmt(stmt, tracker, scope, false).unwrap());
    }
    let num_repeaters = SynLit::Int(LitInt::new(
        &tracker.borrow().num_repeater.to_string(),
        Span::call_site(),
    ));

    // RuleSet Generator closure
    let ruleset_generator = quote!(
        pub fn generate_ruleset(__factory: Rc<RefCell<RuleSetFactory>>) -> Vec<RuleSet> {
            // order corresponds to repeater number
            __factory.borrow_mut().update_ruleset_name(#ruleset_name);
            #(#generated)*
            __factory.borrow().rulesets.clone()
        }
    );
    // fin. Release all scoped values here
    Ok(ruleset_generator)
}

pub(super) fn generate_rule_expr(
    rule_expr: &RuleExpr,
    tracker: &ValueTracker,
) -> IResult<TokenStream> {
    // 0. Get the rule name and register it to the tracker
    let rule_name = &*rule_expr.name.name;
    if tracker.borrow().check_rule_name_exist(rule_name) {
        return Err(RuleSetGenError::RuleNameDuplicationError);
    }
    let scope = rule_name;
    tracker.borrow_mut().register_rule_name(rule_name);
    let rule_name_structure_name = format_ident!("__{}", rule_name);

    // 1. Register repeater identifiers to be used in this RuleExpr
    // e.g. <#rep> This can be refered as repeater value
    // Now the number of repeater is limited to 1
    let repeater_identifier = &*rule_expr.repeater_ident.name;
    tracker
        .borrow_mut()
        .add_internal_repeater_name(rule_name, repeater_identifier);
    // 2. Register given arguments to be used in this RuleExpr
    // e.g.
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
    // match &*rule_expr.ret_type_annotation {
    //     Some(type_annoation) => {
    //         for (type_val, maybe) in &type_annoation.typedefs {
    //             return_types.add_return_type(generate_type_hint(&type_val).unwrap(), *maybe);
    //         }
    //     }
    //     None => {}
    // }
    // tracker
    //     .borrow_mut()
    //     .add_return_type_annotation(rule_name, return_types);

    // 4. Move into the RuleContent
    // This has to be evaluated when the rule is called.
    // As long as the arguments are given, this rule can be executed
    let rule_generator = generate_rule_content(&rule_expr.rule_content, tracker, scope).unwrap();

    Ok(quote!(
        #[derive(Debug, Clone, PartialEq)]
        pub struct #rule_name_structure_name{
            // Order is the same as the def
            pub rule_args: Vec<String>,
            pub callback: Rc<RefCell<RuleSetFactory>>
        }
        impl #rule_name_structure_name{
            pub fn new(ruleset_factory: Rc<RefCell<RuleSetFactory>>) -> Self{
                #rule_name_structure_name{
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

            fn _generate(&mut self, rules: RuleVec, __repeater: &Repeater, __argument: &RuleArgs) -> RuleVec{
                #rule_generator

                rules
            }

            pub fn get_rule_args(&self) -> Vec<String>{
                self.rule_args.clone()
            }

            fn _type_check(){

            }
        }
    ))
}

pub(super) fn generate_rule_content(
    rule_content_expr: &RuleContentExpr,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<TokenStream> {
    // 0. Pre processing (local variable assignment)
    // Variables should be registered in this process
    let mut pre_processing = vec![];
    for let_stmt in &rule_content_expr.pre_processing {
        // register local variables with scope
        pre_processing.push(generate_let(let_stmt, tracker, scope, true).unwrap());
    }

    // 1. condition clause
    let condition_generator =
        generate_cond(&rule_content_expr.condition_expr, tracker, scope, true).unwrap();
    // 2. action clause

    let act_generator =
        generate_act(&*rule_content_expr.action_expr, tracker, scope, true).unwrap();

    // 3. Post processing
    let mut post_processing = vec![];
    for stmt in &rule_content_expr.post_processing {
        post_processing.push(generate_stmt(stmt, tracker, scope, true).unwrap());
    }

    Ok(quote!(
        #(#pre_processing)*
        #condition_generator
        #act_generator
        #(#post_processing)*

    ))
}

pub(super) fn generate_cond(
    cond_expr: &CondExpr,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    let mut clauses = vec![];
    for clause in &cond_expr.clauses {
        let generated = generate_clauses(clause, tracker, scope, in_ruledef).unwrap();
        clauses.push(quote!(
            #generated;
        ))
    }
    let cond_name = match &*cond_expr.name {
        Some(name) => {
            let con_name = &*name.name;
            quote!(#con_name)
        }
        None => {
            quote!(None)
        }
    };
    Ok(quote!(
        let mut condition = Condition::new(#cond_name);
        #(#clauses)*
        // Add this clause to all the rules
        for rule in rules.borrow().iter(){
            rule.borrow_mut().set_condition(condition.clone());
        }
    ))
}

pub(super) fn generate_clauses(
    cond: &CondClauses,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    match cond {
        CondClauses::ResAssign(res_assign) => {
            let generated = generate_res_assign(res_assign, tracker, scope, in_ruledef).unwrap();
            Ok(quote!(#generated;))
        }
        CondClauses::FnCall(fn_call) => {
            let generated = generate_fn_call(fn_call, tracker, scope, in_ruledef).unwrap();
            Ok(quote!(#generated;))
        }
        CondClauses::VariableCallExpr(var_call) => {
            let generated = generate_variable_call(var_call, tracker, scope, in_ruledef).unwrap();
            Ok(quote!(#generated;))
        }
        CondClauses::Comp(comp_expr) => {
            let generated = generate_comp(comp_expr, tracker, scope, in_ruledef).unwrap();
            Ok(quote!(#generated;))
        }
    }
}

pub(super) fn generate_res_assign(
    res_assign: &ResAssign,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    let name = generate_ident(&*res_assign.res_name, tracker, scope).unwrap();
    let fn_call = generate_fn_call(&*res_assign.fn_call, tracker, scope, in_ruledef).unwrap();
    Ok(quote!(let #name = #fn_call))
}

pub(super) fn generate_act(
    act_expr: &ActExpr,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    let mut generated = vec![];
    for stmt in &act_expr.operatable {
        generated.push(generate_stmt(stmt, tracker, scope, in_ruledef).unwrap())
    }
    Ok(quote!(
        #(#generated)*
    ))
}

// Generate stmt expression
// Arguments:
//  stmt: Stmt AST
//  rule_name: Option<String> a name of corresponding Rule
//  ident_tracker: List of identifiers that need to be tracked
pub(super) fn generate_stmt(
    stmt: &Stmt,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    match &*stmt.kind {
        StmtKind::Let(let_stmt) => Ok(generate_let(let_stmt, tracker, scope, in_ruledef).unwrap()),
        StmtKind::Expr(expr) => Ok(generate_expr(expr, tracker, scope, in_ruledef, false).unwrap()),
        StmtKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    }
}

/// Create a new variable that can be used inside the rule
/// Let statement doesn't affect rules. Just update local trackers
pub(super) fn generate_let(
    let_stmt: &Let,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    // Register a variable to the tracker within the ruleset or rule
    match &*let_stmt.ident.type_hint {
        Some(hint) => {
            let variable_name = format_ident!("{}", &*let_stmt.ident.name);
            let (_, type_hint) = generate_type_hint(hint).unwrap();
            let value = generate_expr(&*let_stmt.expr, tracker, scope, in_ruledef, false).unwrap();
            Ok(quote!(
                let #variable_name: #type_hint = #value;
            ))
        }
        None => return Err(RuleSetGenError::NoTypeAnnotationError),
    }
}

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
        ExprKind::Return(return_expr) => Ok(generate_return(return_expr, tracker).unwrap()),
        ExprKind::Send(send_expr) => {
            Ok(generate_send(send_expr, tracker, scope, in_ruledef, in_match).unwrap())
        }
        ExprKind::FnCall(fn_call_expr) => {
            Ok(generate_fn_call(fn_call_expr, tracker, scope, in_ruledef).unwrap())
        }
        ExprKind::RuleCall(rule_call_expr) => {
            Ok(generate_rule_call(rule_call_expr, tracker, scope).unwrap())
        }
        ExprKind::RuLaVec(rula_vec_expr) => {
            Ok(generate_vec(rula_vec_expr, tracker, scope).unwrap())
        }
        ExprKind::RuLaTuple(rula_tuple_expr) => {
            Ok(generate_tuple(rula_tuple_expr, tracker, scope, in_ruledef, in_match).unwrap())
        }
        ExprKind::Comp(comp_expr) => {
            Ok(generate_comp(comp_expr, tracker, scope, in_ruledef).unwrap())
        }
        ExprKind::Term(term_expr) => {
            Ok(generate_term_expr(term_expr, tracker, scope, in_ruledef).unwrap())
        }
        ExprKind::VariableCallExpr(variable_call_expr) => {
            Ok(generate_variable_call(variable_call_expr, tracker, scope, in_ruledef).unwrap())
        }
        ExprKind::Lit(lit_expr) => Ok(generate_lit(&lit_expr, tracker, scope).unwrap()),
        ExprKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    }
}

pub(super) fn generate_if(
    if_expr: &If,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    // If this is not the rule definition, this becomes just an ordinary if expression
    if !in_ruledef {
        // RuleSet def
        let block = generate_expr(&*if_expr.block, tracker, scope, in_ruledef, false).unwrap();
        let mut stmts = vec![];
        for st in &if_expr.stmts {
            stmts.push(generate_stmt(st, tracker, scope, in_ruledef).unwrap())
        }
        let mut elifs = vec![];
        for elif in &if_expr.elif {
            let generated = generate_if(elif, tracker, scope, in_ruledef).unwrap();
            elifs.push(quote!(
                else #generated
            ));
        }
        let mut elses = vec![];
        for els in &*if_expr.els {
            elses.push(generate_stmt(els, tracker, scope, in_ruledef).unwrap())
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
        todo!("under construct : using if expression inside the rule");
        // let block
        Ok(quote!())
    }
}

pub(super) fn generate_for(
    for_expr: &For,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    let block = &for_expr.variables;
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

    let generator_expr = match &*for_expr.generator {
        ForGenerator::Series(series) => {
            generate_series(series, tracker, scope, in_ruledef).unwrap()
        }
        ForGenerator::Expr(expr) => generate_expr(expr, tracker, scope, in_ruledef, false).unwrap(),
        ForGenerator::PlaceHolder => {
            return Err(RuleSetGenError::InitializationError);
        }
    };

    let stmts = &for_expr.stmts;
    let mut generated_stmts = vec![];
    for st in stmts {
        generated_stmts.push(generate_stmt(st, tracker, scope, in_ruledef).unwrap());
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
        Ok(quote!(#func_name (Rc::clone(&rules), #(#generated_arguments),*)))
    } else {
        Ok(quote!(#func_name ( #(#generated_arguments),*)))
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

pub(super) fn generate_rule_call(
    rule_call_expr: &RuleCall,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<TokenStream> {
    // 0. check the rule name if the definition exists
    let rule_name = &*rule_call_expr.rule_name.name;
    if !tracker.borrow().check_rule_name_exist(rule_name) {
        return Err(RuleSetGenError::NoRuleFoundError);
    }

    // At this point, repeater argument and ordinary argument should be resolved
    // Currently the number of repater for one rule is limited to 1
    let (repeater_val, repeater_index) =
        generate_rep_call_arg(&*rule_call_expr.repeater_arg, tracker, scope, false).unwrap();

    // 0. get argument identifiers
    let mut arguments = vec![];
    for arg in &rule_call_expr.argument {
        arguments.push(generate_fn_call_arg(arg, tracker, scope, false).unwrap());
    }
    // 1. get argument types to wrap with Types enum (type error should be caught at this moment)
    // Check (supposed) argument types
    let supposed_arguments = tracker.borrow().get_rule_arguments(rule_name).clone();

    // Check the number of arguments does match
    if arguments.len() != supposed_arguments.len() {
        return Err(RuleSetGenError::ArgumentNumberError);
    }

    // 2. register arguments to rules (rule_name, arguments)
    let mut argument_register = vec![];
    for (gen_arg, (_, type_val)) in arguments.iter().zip(supposed_arguments.iter()) {
        let generated_type = match type_val {
            Types::Boolean => {
                quote!(RuLaValue::Boolean(#gen_arg as bool))
            }
            Types::Int => {
                quote!(RuLaValue::Int(#gen_arg as i64))
            }
            Types::UInt => {
                quote!(RuLaValue::UInt(#gen_arg as u64))
            }
            Types::Float => {
                quote!(RuLaValue::Float(#gen_arg as f64))
            }
            Types::Str => {
                quote!(RuLaValue::Str(#gen_arg as String))
            }
            Types::Result => {
                quote!(RuLaValue::Result(#gen_arg))
            }
            Types::Qubit => {
                quote!(RuLaValue::Qubit(#gen_arg))
            }
            Types::Message => {
                quote!(RuLaValue::Message(#gen_arg))
            }
            _ => todo!("Is this supposed to be error?"),
        };
        argument_register.push(generated_type)
    }
    // 3. resolve arguments with internal name
    // 4. flush the arguments
    // Add rule to stage inside the RuleSet
    Ok(quote!(
        // register current argument
        __factory.borrow_mut().register_args(#rule_name, vec![#(#argument_register),*]);
        __factory.borrow_mut().resolve_args(#rule_name);
        // Register arguments
        let _stage = __factory.borrow_mut().genenerate_stage(#rule_name, #repeater_index);
        // Flush Arguments
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
pub(super) fn generate_return(
    return_expr: &Return,
    tracker: &ValueTracker,
) -> IResult<TokenStream> {
    todo!();
    Ok(quote!())
    // Ok(quote!(return rules))
}

pub(super) fn generate_send(
    send_expr: &Send,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
    in_match: bool,
) -> IResult<TokenStream> {
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
    if in_match {
        Ok(quote!(
            send(Rc::clone(&__temp_rules), &#expr, #message_type);
        ))
    } else {
        Ok(quote!(
            send(Rc::clone(&rules), &#expr, #message_type);
        ))
    }
}

// Generate Match expression to achieve match action
// In the case of static generation, this expand Rules to the stages
pub(super) fn generate_match(
    match_expr: &Match,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    let expr = generate_expr(&*match_expr.expr, tracker, scope, in_ruledef, false).unwrap();
    let mut match_conditions = vec![];
    let mut match_actions = vec![];
    for arm in &match_expr.match_arms {
        let (mc, ma) = generate_match_arm(arm, tracker, scope, in_ruledef).unwrap();
        match_conditions.push(mc);
        match_actions.push(ma);
    }
    let mut final_rule_count = match_conditions.len();

    let otherwise = match &*match_expr.otherwise {
        Some(other) => {
            let generated = generate_match_action(other, tracker, scope, in_ruledef).unwrap();
            final_rule_count += 1;
            quote!(
                // Define new rules to flush the current rules
                let mut rules = Rc::new(RefCell::new(vec![]));
                for new_rule in new_rule_vec{
                    rules.borrow_mut().push(new_rule);
                }

                let mut fin_rule_stack = vec![];
                for fin_rule in &*finally_rules.borrow_mut(){
                    let generated_vec = (|__new_rule|{
                        let __temp_rules = Rc::new(RefCell::new(vec![RefCell::new(__new_rule)]));
                        #generated;
                        __temp_rules
                    })(fin_rule.borrow().clone());
                    for gen_rule in &*generated_vec.borrow(){
                        fin_rule_stack.push(gen_rule.clone());
                    }
                }
                for finally_rule in &fin_rule_stack{
                    rules.borrow_mut().push(finally_rule.clone());
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

        let __match_conditions = vec![
            #(#match_conditions),*
        ];
        let __match_actions: Vec<Box<dyn Fn(Rule) -> (RuleVec)>> = vec![
            #(#match_actions),*
        ];

        let finally_rules = &*rules.clone();

        let mut new_rule_vec = vec![];
        for rule in &*rules.borrow_mut(){
            for ((__cmp_op, __val), __action_func) in &mut __match_conditions.iter().zip(&__match_actions){
                let mut cloned_rule = rule.borrow().clone();
                cloned_rule.add_condition_clause(ConditionClauses::Cmp(Cmp::new(__cmp_op.clone(), __val.clone())));
                let generated_rules = __action_func(cloned_rule);
                for gen_rule in &*generated_rules.borrow(){
                    new_rule_vec.push(gen_rule.clone());
                }
            }
        }
        #otherwise

        let num_rules = rules.borrow().len();
        if  num_rules != final_rule_count {
            panic!("The final rule size is wrong Suppose: {} != Actual{}", final_rule_count, num_rules);
        }


    ))
}

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
           (CmpOp::Eq, __cmp_target(#match_condition))
        ),
        quote!(
            Box::new(|__new_rule|{
                let __temp_rules = Rc::new(RefCell::new(vec![RefCell::new(__new_rule)]));
                #match_actions;
                __temp_rules
            })
        ),
    ))
}

pub(super) fn generate_match_condition(
    match_condition: &MatchCondition,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    // Right now, satisfiable can only take literals, but in the future, this should be more flexible
    match &*match_condition.satisfiable {
        Satisfiable::Lit(literal) => Ok(generate_lit(literal, tracker, scope).unwrap()),
        _ => Err(RuleSetGenError::InitializationError),
    }
}

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

pub(super) fn generate_comp(
    comp_expr: &Comp,
    tracker: &ValueTracker,
    scope: &Scope,
    in_ruledef: bool,
) -> IResult<TokenStream> {
    let lhs = generate_expr(&comp_expr.lhs, tracker, scope, in_ruledef, false).unwrap();
    let rhs = generate_expr(&comp_expr.rhs, tracker, scope, in_ruledef, false).unwrap();
    let (op, cmp_op) = match *comp_expr.comp_op {
        CompOpKind::Lt => (quote!(<), quote!(__CmpOp::Lt)),
        CompOpKind::Gt => (quote!(>), quote!(__CmpOp::Gt)),
        CompOpKind::LtE => (quote!(<=), quote!(__CmpOp::LtE)),
        CompOpKind::GtE => (quote!(>=), quote!(__CmpOp::GtE)),
        CompOpKind::Eq => (quote!(==), quote!(__CmpOp::Eq)),
        CompOpKind::Nq => (quote!(!=), quote!(__CmpOp::Nq)),
        CompOpKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    };
    Ok(quote!(#lhs #op #rhs))
}

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
        _ => todo!("{:#?} not yet implemented", lit),
    }
}

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

pub(super) fn generate_int_lit(integer_lit: &IntegerLit) -> IResult<TokenStream> {
    let value = SynLit::Int(LitInt::new(&integer_lit.value.name, Span::call_site()));
    if integer_lit.negative {
        Ok(quote!(-#value))
    } else {
        Ok(quote!(#value))
    }
}

pub(super) fn generate_float_lit(float_lit: &FloatLit) -> IResult<TokenStream> {
    let value = SynLit::Float(LitFloat::new(&float_lit.value.name, Span::call_site()));
    if float_lit.negative {
        Ok(quote!(-#value))
    } else {
        Ok(quote!(#value))
    }
}

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
            Ok(generate_term_expr(term_expr, tracker, scope, in_ruledef).unwrap())
        }
        Terms::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    }
}

// Generate identifier token from Ident ast
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
        Types::Vec(inner_def) => {
            todo!();
            Ok(quote!(eval_as_vec()))
        }
        Types::Unknown => Err(RuleSetGenError::UnknownTypeError),
    }
}
