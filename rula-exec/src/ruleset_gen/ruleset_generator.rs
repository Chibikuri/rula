use super::conf_parser::parse_config;
use super::error::RuleSetGenError;
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

type RuleVec = Rc<RefCell<Vec<Rule>>>;
type LocalValTrack = Rc<RefCell<LocalVariableTracker>>;

pub type Scope = String;
pub type Closure<T, U, V> = Box<dyn Fn(T, U) -> V>;
pub type ValueTracker = RefCell<Tracker>;

/// Generate corresponding rust code from ast
/// Every nested generators returns a piece of TokenStream
/// Arguments:
///     ast_tree
pub fn generate(ast_tree: &AstNode, config_path: PathBuf) -> IResult<TokenStream> {
    // RuLa should know how many repeaters inconfig at this moment

    // Initialize tracker to track global state over the functions
    let tracker = RefCell::new(Tracker::new());

    let num_node = parse_config(config_path).unwrap().len();
    tracker.borrow_mut().update_num_node(num_node as u32);
    // Add empty RuleSets
    for i in 0..num_node {
        tracker.borrow_mut().add_ruleset(i, RuleSet::new("empty"));
    }
    // Generated rula program
    let tokens = match ast_tree {
        // All RuLa AST starts RuLa Node
        AstNode::RuLa(rula) => {
            let generated = generate_rula(rula, &tracker).unwrap();
            quote!(
                use rula_lib as rula_std;
                use rula_exec::ruleset_gen::ruleset::{RuleSet, Rule, Stage};
                use rula_exec::ruleset_gen::condition::*;
                use rula_exec::ruleset_gen::action::*;

                use std::rc::Rc;
                use std::cell::RefCell;
                use rula_std::prelude::*;
                #[allow(unused)]
                mod rula{
                    use super::*;

                    type RepeaterNumber = usize;
                    #generated
                }

                #[allow(unused)]
                pub fn generate_ruleset(){
                    let rulesets = rula::ruleset_gen();
                }

                #[cfg(test)]
                mod tests{
                    #[test]
                    fn test_rule_generate(){
                        assert_eq!(1, 1);
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
    // How to get the other rules?
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
                let path_fragment = generate_ident(&new_ident, tracker).unwrap();
                single_path.push(path_fragment)
            }
            let path_head = &single_path[0];
            let path_left = &single_path[1..];
            quote!(
                #path_head #(::#path_left)*
            )
        } else {
            let path_ident = generate_ident(path_ident, tracker).unwrap();
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
    if tracker.borrow().check_rule_name_exist(ruleset_name) {
        return Err(RuleSetGenError::SameNameExistInRuleError);
    }
    // Register ruleset name no to be used by other rule as well
    tracker.borrow_mut().register_rule_name(&ruleset_name);
    tracker.borrow_mut().update_ruleset_name(ruleset_name);

    let mut generated = vec![];
    for stmt in &ruleset_expr.rules {
        generated.push(generate_stmt(stmt, tracker).unwrap());
    }
    let num_repeaters = SynLit::Int(LitInt::new(
        &tracker.borrow().num_repeater.to_string(),
        Span::call_site(),
    ));

    // RuleSet Generator closure
    let ruleset_generator = quote!(
        pub fn ruleset_gen(repeaters: RepeaterList) -> Vec<RuleSet> {
            // order corresponds to repeater number
            let mut rulesets = vec![];
            for i in 0..#num_repeaters{
                rulesets.insert(i, RuleSet::new("empty"));
            }
            #(#generated)*
            rulesets
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
    tracker.borrow_mut().register_rule_name(&rule_name);

    // Introduce scope here
    let scope = rule_name;

    // 1. Register repeater identifiers to be used in this RuleExpr
    // e.g. <#rep> This can be refered as repeater value
    // Now the number of repeater is limited to 1
    let repeater_identifier = &*rule_expr.repeater_ident.name;
    tracker
        .borrow_mut()
        .add_internal_repeater_name(rule_name, repeater_identifier);
    // 2. Register given arguments to be used in this RuleExpr
    // e.g.
    let argument_identifiers = {
        let mut argument_names = vec![];
        for arg in &*rule_expr.args {
            argument_names.push(arg.name.to_string());
        }
        argument_names
    };
    tracker
        .borrow_mut()
        .add_internal_argument_names(rule_name, argument_identifiers);

    // 3. Check if there is a type annotation for return value or not
    let mut return_types = RetTypeAnnotation::new(scope);
    match &*rule_expr.ret_type_annotation {
        Some(type_annoation) => {
            for (type_val, maybe) in &type_annoation.typedefs {
                return_types.add_return_type(generate_type_hint(&type_val).unwrap(), *maybe);
            }
        }
        None => {}
    }
    tracker
        .borrow_mut()
        .add_return_type_annotation(rule_name, return_types);

    // 4. Move into the RuleContent
    // This has to be evaluated when the rule is called.
    // As long as the arguments are given, this rule can be executed
    let rule_generator = generate_rule_content(&rule_expr.rule_content, tracker, scope).unwrap();

    let rule_generator = move |repeater: Repeater,
                               arguments: Arguments,
                               tracker: &ValueTracker,
                               scope: &Scope| {
        let local_tracker = Rc::new(RefCell::new(LocalVariableTracker::new()));
        // Currently only one repeater can be inside this
        local_tracker.borrow_mut().register(
            &tracker.borrow().get_internal_repeater_name(&scope),
            &RuLaValue::Repeater(repeater),
        );

        let num_args = arguments.vals.len();
        if num_args != tracker.borrow().get_internal_argument_names(&scope).len() {
            panic!("Something Failed internally. The number of arguments doesn't match it is supposed to be");
        }
        for (index, arg_name) in tracker
            .borrow()
            .get_internal_argument_names(&scope)
            .iter()
            .enumerate()
        {
            local_tracker
                .borrow_mut()
                .register(arg_name, &arguments.vals[index])
        }
        // Initial RuleSettings
        // The number of rules would be larger and larger
        let rules = Rc::new(RefCell::new(vec![Rule::new(&scope)]));
        let generated_rules = rule_generator(Rc::clone(&rules), Rc::clone(&local_tracker));
        let mut stage = Stage::new();
        for rl in &*generated_rules.borrow() {
            stage.add_rule(rl.clone());
        }
        stage
    };

    // 5. Release all the scope within this Rule
    // tracker.borrow_mut().clean_scope(scope);
    Ok(quote!())
}

pub(super) fn generate_rule_content(
    rule_content_expr: &RuleContentExpr,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<Closure<RuleVec, LocalValTrack, RuleVec>> {
    // 0. Pre processing (local variable assignment)
    // Variables should be registered in this process
    // for let_stmt in &rule_content_expr.pre_processing {
    //     // register local variables with scope
    //     generate_let(let_stmt, tracker, scope).unwrap();
    // }

    // 1. condition clause
    let condition_generator =
        generate_cond(&rule_content_expr.condition_expr, tracker, scope).unwrap();
    // 2. action clause

    let act_generator = generate_act(&*rule_content_expr.action_expr, tracker, scope).unwrap();

    // 3. Post processing
    let rule_content_generator = move |rules: RuleVec, local_val_tracker: LocalValTrack| {
        // Prepare local value tracker to track the local varibale
        // 0. value assignment with current tracker
        // Right now, all arguments are cloned but this should be cleaned with Rc and RefCell in the future
        condition_generator(Rc::clone(&rules), Rc::clone(&local_val_tracker));
        act_generator(Rc::clone(&rules), Rc::clone(&local_val_tracker));
        rules
    };

    Ok(Box::new(rule_content_generator))
}

pub(super) fn generate_cond(
    cond_expr: &CondExpr,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<Closure<RuleVec, LocalValTrack, RuleVec>> {
    Ok(Box::new(|rules, local_tracker| rules))
}

pub(super) fn generate_act(
    act_expr: &ActExpr,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<Closure<RuleVec, LocalValTrack, RuleVec>> {
    Ok(Box::new(|rules, var_tracker| rules))
}

// Generate stmt expression
// Arguments:
//  stmt: Stmt AST
//  rule_name: Option<String> a name of corresponding Rule
//  ident_tracker: List of identifiers that need to be tracked
pub(super) fn generate_stmt(stmt: &Stmt, tracker: &ValueTracker) -> IResult<TokenStream> {
    match &*stmt.kind {
        StmtKind::Let(let_stmt) => Ok(generate_let(let_stmt, tracker).unwrap()),
        StmtKind::Expr(expr) => Ok(generate_expr(expr, tracker).unwrap()),
        StmtKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    }
}

/// Create a new variable that can be used inside the rule
/// Let statement doesn't affect rules. Just update local trackers
pub(super) fn generate_let(let_stmt: &Let, tracker: &ValueTracker) -> IResult<TokenStream> {
    // Register a variable to the tracker within the ruleset or rule
    let value = generate_expr(&*let_stmt.expr, tracker).unwrap();
    let variable_name = &*let_stmt.ident.name;
    match &*let_stmt.ident.type_hint {
        Some(hint) => {
            // tracker.borrow_mut().register_local_variable(
            //     variable_name,
            //     Variable::new(
            //         variable_name,
            //         scope,
            //         VarKind::LetAssignment,
            //         generate_type_hint(hint).unwrap(),
            //     ),
            // );
        }
        None => return Err(RuleSetGenError::NoTypeAnnotationError),
    }
    Ok(quote!())
}

pub(super) fn generate_expr(expr: &Expr, tracker: &ValueTracker) -> IResult<TokenStream> {
    match &*expr.kind {
        ExprKind::If(if_expr) => Ok(generate_if(if_expr, tracker).unwrap()),
        ExprKind::For(for_expr) => Ok(generate_for(for_expr, tracker).unwrap()),
        ExprKind::Match(match_expr) => Ok(generate_match(match_expr, tracker).unwrap()),
        ExprKind::Return(return_expr) => Ok(generate_return(return_expr, tracker).unwrap()),
        ExprKind::Send(send_expr) => Ok(generate_send(send_expr, tracker).unwrap()),
        ExprKind::FnCall(fn_call_expr) => Ok(generate_fn_call(fn_call_expr, tracker).unwrap()),
        ExprKind::RuleCall(rule_call_expr) => {
            Ok(generate_rule_call(rule_call_expr, tracker).unwrap())
        }
        ExprKind::Comp(comp_expr) => Ok(generate_comp(comp_expr, tracker).unwrap()),
        ExprKind::Term(term_expr) => Ok(generate_term_expr(term_expr, tracker).unwrap()),
        ExprKind::VariableCallExpr(variable_call_expr) => {
            Ok(generate_variable_call(variable_call_expr, tracker).unwrap())
        }
        ExprKind::Lit(lit_expr) => Ok(generate_lit(&lit_expr, tracker, "".to_string()).unwrap()),
        ExprKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    }
}

pub(super) fn generate_if(if_expr: &If, tracker: &ValueTracker) -> IResult<TokenStream> {
    Ok(quote!())
}

pub(super) fn generate_for(for_expr: &For, tracker: &ValueTracker) -> IResult<TokenStream> {
    let block = &for_expr.variables;
    let generated_block = if block.len() > 1 {
        let mut generated = vec![];
        for iden in block {
            generated.push(generate_ident(&iden, tracker).unwrap());
        }
        quote!(
            (#(#generated),*)
        )
    } else {
        // len == 1
        let generated = generate_ident(&block[0], tracker).unwrap();
        quote!(#generated)
    };

    let generator_expr = match &*for_expr.generator {
        ForGenerator::Series(series) => generate_series(series, tracker).unwrap(),
        ForGenerator::Expr(expr) => generate_expr(expr, tracker).unwrap(),
        ForGenerator::PlaceHolder => {
            return Err(RuleSetGenError::InitializationError);
        }
    };

    let stmts = &for_expr.stmts;
    let mut generated_stmts = vec![];
    for st in stmts {
        generated_stmts.push(generate_stmt(st, tracker).unwrap());
    }
    Ok(quote!(
        for #generated_block in #generator_expr{
            #(#generated_stmts)*
        }
    ))
}

pub(super) fn generate_series(series: &Series, tracker: &ValueTracker) -> IResult<TokenStream> {
    let start_val = generate_number_lit(&*series.start, tracker, "int".to_string()).unwrap();
    let end_val = generate_expr(&*series.end, tracker).unwrap();
    Ok(quote!(#start_val..#end_val))
}
pub(super) fn generate_fn_call(
    fn_call_expr: &FnCall,
    tracker: &ValueTracker,
) -> IResult<TokenStream> {
    // Before generating functions, check function table to check whether it's properly defined or not
    Ok(quote!())
}

pub(super) fn generate_fn_call_arg(
    fn_call_arg: &FnCallArgs,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<TokenStream> {
    Ok(quote!())
    // Ok(RuLaValue::Str(String::from("")))
}

pub(super) fn generate_rule_call(
    rule_call_expr: &RuleCall,
    tracker: &ValueTracker,
) -> IResult<TokenStream> {
    // 0. check the rule name if the definition exists
    let rule_name = &*rule_call_expr.rule_name.name;
    if !tracker.borrow().check_rule_name_exist(rule_name) {
        return Err(RuleSetGenError::NoRuleFoundError);
    }

    let scope = rule_name;
    // At this point, repeater argument and ordinary argument should be resolved
    // Currently the number of repater for one rule is limited to 1
    let repeater_index =
        generate_rep_call_arg(&*rule_call_expr.repeater_arg, tracker, scope).unwrap();
    // let repeater_arg = &tracker.borrow().repeaters[repeater_index];
    // let mut arguments = Arguments::place_holder();
    // arguments.update_scope(scope);
    // for arg in &rule_call_expr.argument {
    //     arguments.add_val(generate_fn_call_arg(arg, tracker, scope).unwrap().clone());
    // }

    // // Check argument types, argument numbers, values
    // // let stage = rule_evaluator(rule_name, repeater_arg, &arguments, tracker).unwrap();

    // let stage = tracker
    //     .borrow()
    //     .eval_rule(rule_name, repeater_arg, &arguments, tracker, scope);

    // // Add stage to the ruleset
    // // At this moment, repeater index must be corresponding to where this rule is added
    // tracker.borrow().add_stage(repeater_index, stage);

    // Add rule to stage inside the RuleSet
    Ok(quote!())
}

// Repeater call must be casted to usize to
pub(super) fn generate_rep_call_arg(
    repeater_call_arg: &RepeaterCallArg,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<TokenStream> {
    let index: usize = match repeater_call_arg {
        RepeaterCallArg::Term(term) => 0,
        RepeaterCallArg::Ident(ident) => 0,
        RepeaterCallArg::NumberLit(number_lit) => {
            let val = &*number_lit.value.name.clone();
            val.parse::<usize>().unwrap()
        }
        RepeaterCallArg::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    };
    // Ok(index as usize)
    Ok(quote!())
}

// Promote resources to the next stage
pub(super) fn generate_return(
    return_expr: &Return,
    tracker: &ValueTracker,
) -> IResult<TokenStream> {
    Ok(quote!())
}

pub(super) fn generate_send(send_expr: &Send, tracker: &ValueTracker) -> IResult<TokenStream> {
    Ok(quote!())
}

// Generate Match expression to achieve match action
// In the case of static generation, this expand Rules to the stages
pub(super) fn generate_match(match_expr: &Match, tracker: &ValueTracker) -> IResult<TokenStream> {
    Ok(quote!())
}

pub(super) fn generate_match_arm(
    match_arm: &MatchArm,
    tracker: &ValueTracker,
) -> IResult<TokenStream> {
    // let generated_match_condition =
    //     generate_match_condition(&mut *match_arm.condition, ident_tracker).unwrap();
    // let generated_match_action =
    //     generate_match_action(&mut *match_arm.action, ident_tracker, false, false).unwrap();
    // Ok(quote!(#generated_match_condition => {#generated_match_action}))
    Ok(quote!())
}

pub(super) fn generate_match_condition(
    match_condition: &MatchCondition,
    tracker: &ValueTracker,
) -> IResult<TokenStream> {
    // Right now, satisfiable can only take literals, but in the future, this should be more flexible
    match &*match_condition.satisfiable {
        Satisfiable::Lit(literal) => Ok(generate_lit(literal, tracker, "int".to_string()).unwrap()),
        _ => Err(RuleSetGenError::InitializationError),
    }
}

pub(super) fn generate_match_action(
    match_action: &MatchAction,
    ident_tracker: &ValueTracker,
) -> IResult<TokenStream> {
    // let mut actionables = vec![];
    // for actionable in &mut *match_action.actionable {
    //     actionables.push(
    //         generate_expr(
    //             actionable,
    //             None,
    //             ident_tracker,
    //             true,
    //             in_static,
    //             in_closure,
    //             false,
    //         )
    //         .unwrap(),
    //     );
    // }
    // Ok(quote!(
    //     #(#actionables);*
    // ))
    Ok(quote!())
}

pub(super) fn generate_comp(comp_expr: &Comp, tracker: &ValueTracker) -> IResult<TokenStream> {
    let lhs = generate_expr(&comp_expr.lhs, tracker).unwrap();
    let rhs = generate_expr(&comp_expr.rhs, tracker).unwrap();
    let (op, cmp_op) = match *comp_expr.comp_op {
        CompOpKind::Lt => (quote!(<), quote!(__CmpOp::Lt)),
        CompOpKind::Gt => (quote!(>), quote!(__CmpOp::Gt)),
        CompOpKind::LtE => (quote!(<=), quote!(__CmpOp::LtE)),
        CompOpKind::GtE => (quote!(>=), quote!(__CmpOp::GtE)),
        CompOpKind::Eq => (quote!(==), quote!(__CmpOp::Eq)),
        CompOpKind::Nq => (quote!(!=), quote!(__CmpOp::Nq)),
        CompOpKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    };
    Ok(quote!())
}

pub(super) fn generate_variable_call(
    variable_call_expr: &VariableCallExpr,
    tracker: &ValueTracker,
) -> IResult<TokenStream> {
    let mut generated = vec![];

    for callable in &variable_call_expr.variables {
        match callable {
            Callable::FnCall(fn_call) => {
                generated.push(generate_fn_call(fn_call, tracker).unwrap());
            }
            Callable::Ident(ident) => generated.push(generate_ident(ident, tracker).unwrap()),
            Callable::RepeaterIdent(ident) => {
                // generated.push()
            }
        }
    }
    Ok(quote!(#(#generated)*))
}

pub(super) fn generate_lit(
    lit: &Lit,
    tracker: &ValueTracker,
    cast: String,
) -> IResult<TokenStream> {
    match &*lit.kind {
        LitKind::Ident(ident_lit) => Ok(generate_ident(ident_lit, tracker).unwrap()),
        LitKind::NumberLit(number_lit) => {
            Ok(generate_number_lit(number_lit, tracker, cast).unwrap())
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
    cast: String,
) -> IResult<TokenStream> {
    if &cast == "int" {
        let value = SynLit::Int(LitInt::new(&number_lit.value.name, Span::call_site()));
        Ok(quote!(0))
    } else {
        let value = SynLit::Float(LitFloat::new(&*number_lit.value.name, Span::call_site()));
        Ok(quote!(#value))
    }
}

pub(super) fn generate_term_expr(term_expr: &Term, tracker: &ValueTracker) -> IResult<TokenStream> {
    // We could make Term struct instead of direct calc
    // For now, this function just returns calc result as f64
    // let val = LitFloat::new(&term_expr.to_string(), Span::call_site());
    let lhs = generate_term_element(&term_expr.lhs, tracker).unwrap();
    let rhs = generate_term_element(&term_expr.rhs, tracker).unwrap();
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

pub(super) fn generate_term_element(term: &Terms, tracker: &ValueTracker) -> IResult<TokenStream> {
    match term {
        Terms::FnCall(fn_call_expr) => Ok(generate_fn_call(fn_call_expr, tracker).unwrap()),
        Terms::VariableCallExpr(var_call_expr) => {
            Ok(generate_variable_call(var_call_expr, tracker).unwrap())
        }
        Terms::Lit(lit_expr) => Ok(generate_lit(lit_expr, tracker, "".to_string()).unwrap()),
        Terms::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    }
}

// Generate identifier token from Ident ast
pub(super) fn generate_ident(ident: &Ident, tracker: &ValueTracker) -> IResult<TokenStream> {
    if &*ident == &Ident::place_holder() {
        return Err(RuleSetGenError::InitializationError);
    }

    let identifier = format_ident!("{}", *ident.name);
    Ok(quote!(#identifier))
}

pub(super) fn generate_type_hint(type_hint: &TypeDef) -> IResult<Types> {
    match type_hint {
        TypeDef::Repeater => Ok(Types::Repeater),
        TypeDef::Boolean => Ok(Types::Boolean),
        TypeDef::Integer => Ok(Types::Int),
        TypeDef::UnsignedInteger => Ok(Types::UInt),
        TypeDef::Float => Ok(Types::Float),
        TypeDef::Str => Ok(Types::Str),
        TypeDef::Qubit => Ok(Types::Qubit),
        TypeDef::Vector(inner) => {
            let inner_type = generate_type_hint(inner).unwrap();
            Ok(Types::Vec(Box::new(inner_type)))
        }
        TypeDef::PlaceHolder => return Err(RuleSetGenError::InitializationError),
        _ => todo!("type {:#?} not yet implemented", type_hint),
    }
}
