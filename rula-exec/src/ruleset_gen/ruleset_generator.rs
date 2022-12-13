use super::action::Action;
use super::condition::Condition;
// This is entory point to generate code from AST
use super::conf_parser::parse_config;
use super::error::RuleSetGenError;
use super::ruleset::{Rule, RuleSet, Stage};
use super::tracker::{Arguments, RetTypeAnnotation, Tracker, VarKind, Variable};
use super::types::*;
use super::IResult;
use rula_parser::parser::ast::*;

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;

use proc_macro2::{Span, TokenStream};
use syn::Lit as SynLit;
use syn::{LitFloat, LitStr};

type Scope = String;
type SingleClosure<T, U> = Box<dyn Fn(T) -> U>;
pub type Closure<T, U, V> = Box<dyn Fn(T, U) -> V>;
pub type ValueTracker = RefCell<Tracker>;
/// Generate corresponding rust code from ast
/// Every nested generators returns a piece of TokenStream
/// Arguments:
///     ast_tree
pub fn generate(ast_tree: &AstNode, config_path: PathBuf) -> IResult<Vec<RuleSet>> {
    // RuLa should know how many repeaters inconfig at this moment

    // Initialize tracker to track global state over the functions
    let tracker = RefCell::new(Tracker::new());

    parse_config(config_path, &tracker);
    let num_node = tracker.borrow().repeaters.len();
    // Add empty RuleSets
    for i in 0..num_node {
        tracker.borrow_mut().add_ruleset(i, RuleSet::new("empty"));
    }
    // Generated rula program
    match ast_tree {
        // All RuLa AST starts RuLa Node
        AstNode::RuLa(rula) => generate_rula(rula, &tracker).unwrap(),
        AstNode::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    };
    // return generated rulesets
    let rulesets = tracker.borrow().return_rulesets();
    Ok(rulesets)
}

// Generate entire rula program
pub(super) fn generate_rula(rula: &RuLa, tracker: &ValueTracker) -> IResult<()> {
    match &*rula.rula {
        RuLaKind::Program(program) => {
            generate_program(program, tracker).unwrap();
        }
        RuLaKind::Ignore => {}
        RuLaKind::Eoi => {}
        RuLaKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    }
    Ok(())
}

// Generate progra
// program: Program AST that contains a vector of Stmt
pub(super) fn generate_program(program: &Program, tracker: &ValueTracker) -> IResult<()> {
    // A vector to store a set of generated stmts
    for program_kind in &program.programs {
        // Right now, a program can takes a stmt
        match program_kind {
            ProgramKind::Repeaters => {
                // Prepare annotation
            }
            ProgramKind::Import(import_expr) => {
                generate_import(import_expr, tracker).unwrap();
            }
            ProgramKind::RuleSetExpr(ruleset_expr) => {
                generate_ruleset_expr(ruleset_expr, tracker).unwrap();
            }
            ProgramKind::RuleExpr(rule_expr) => {
                generate_rule_expr(rule_expr, tracker).unwrap();
            }
        }
    }
    Ok(())
}

pub(super) fn generate_import(import_expr: &Import, tracker: &ValueTracker) -> IResult<()> {
    // How to get the other rules?
    Ok(())
}

pub(super) fn generate_ruleset_expr(
    ruleset_expr: &RuleSetExpr,
    tracker: &ValueTracker,
) -> IResult<()> {
    // 0. Get the ruleset name and update the ruleset names
    let ruleset_name = &*ruleset_expr.name.name;
    if tracker.borrow().check_rule_name_exist(ruleset_name) {
        return Err(RuleSetGenError::SameNameExistInRuleError);
    }
    // Register ruleset name no to be used by other rule as well
    tracker.borrow_mut().register_rule_name(&ruleset_name);
    tracker.borrow_mut().update_ruleset_name(ruleset_name);

    // 1. Introduce ruleset scope
    let scope = ruleset_name;
    // Generate RuleSet
    for stmt in &ruleset_expr.rules {
        generate_stmt(stmt, tracker, scope).unwrap();
    }
    // fin. Release all scoped values here
    Ok(())
}

pub(super) fn generate_rule_expr(rule_expr: &RuleExpr, tracker: &ValueTracker) -> IResult<()> {
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
    // In the rulecall, this argument must be properly resolved.
    // Prepare the closure for repeaters to be called later and store it in tracker.
    tracker.borrow_mut().register_local_variable(
        repeater_identifier,
        Variable::new(
            repeater_identifier,
            scope,
            VarKind::RepeaterArgument,
            Types::Repeater,
        ),
    );

    // 2. Register given arguments to be used in this RuleExpr
    // e.g.
    for arg in &rule_expr.args {
        match &*arg.type_hint {
            Some(hint) => {
                tracker.borrow_mut().register_local_variable(
                    &arg.name,
                    Variable::new(
                        &arg.name,
                        scope,
                        VarKind::Argument,
                        generate_type_hint(hint).unwrap(),
                    ),
                );
            }
            None => return Err(RuleSetGenError::NoTypeAnnotationError),
        }
    }

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

    // 4. Move into the RuleContent
    // This has to be evaluated when the rule is called.
    // As long as the arguments are given, this rule can be executed
    let rule_generator = generate_rule_content(&rule_expr.rule_content, tracker, scope).unwrap();
    tracker
        .borrow_mut()
        .add_unresolved_rule::<Closure<Repeater, Arguments, Stage>>(rule_name, rule_generator);

    // 5. Release all the scope within this Rule
    tracker.borrow_mut().clean_scope(scope);
    Ok(())
}

pub(super) fn generate_rule_content(
    rule_content_expr: &RuleContentExpr,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<Closure<Repeater, Arguments, Stage>> {
    // 0. Pre processing (local variable assignment)
    // Variables should be registered in this process
    for let_stmt in &rule_content_expr.pre_processing {
        // register local variables with scope
        generate_let(let_stmt, tracker, scope).unwrap();
    }

    // 1. condition clause
    let condition_generator =
        generate_cond(&rule_content_expr.condition_expr, tracker, scope).unwrap();
    // 2. action clause

    let act_generator = generate_act(&*rule_content_expr.action_expr, tracker, scope).unwrap();

    // 3. Post processing
    let rule_generator = move |repeater: Repeater, arguments: Arguments| {
        let mut stage = Stage::new();
        let mut rule = Rule::new("");
        // Right now, all arguments are cloned but this should be cleaned with Rc and RefCell in the future
        rule.set_condition(condition_generator(repeater.clone(), arguments.clone()));
        rule.set_action(act_generator(repeater.clone(), arguments.clone()));
        stage.add_rule(rule);
        stage
    };

    Ok(Box::new(rule_generator))
}

pub(super) fn generate_cond(
    cond_expr: &CondExpr,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<Closure<Repeater, Arguments, Condition>> {
    Ok(Box::new(|repeater, arguments| Condition::new(None)))
}

pub(super) fn generate_act(
    act_expr: &ActExpr,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<Closure<Repeater, Arguments, Action>> {
    Ok(Box::new(|repeater, arguments| Action::new(None)))
}

// Generate stmt expression
// Arguments:
//  stmt: Stmt AST
//  rule_name: Option<String> a name of corresponding Rule
//  ident_tracker: List of identifiers that need to be tracked
pub(super) fn generate_stmt(stmt: &Stmt, tracker: &ValueTracker, scope: &Scope) -> IResult<()> {
    match &*stmt.kind {
        StmtKind::Let(let_stmt) => {
            generate_let(let_stmt, tracker, scope).unwrap();
        }
        StmtKind::Expr(expr) => {
            generate_expr(expr, tracker, scope).unwrap();
        }
        StmtKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    }
    Ok(())
}

/// Create a new variable that can be used inside the rule
/// Store the variable in the tracker
pub(super) fn generate_let(let_stmt: &Let, tracker: &ValueTracker, scope: &Scope) -> IResult<()> {
    // Register a variable to the tracker within the ruleset or rule
    let variable_name = &*let_stmt.ident.name;
    match &*let_stmt.ident.type_hint {
        Some(hint) => {
            tracker.borrow_mut().register_local_variable(
                variable_name,
                Variable::new(
                    variable_name,
                    scope,
                    VarKind::LetAssignment,
                    generate_type_hint(hint).unwrap(),
                ),
            );
            let value = generate_expr(&*let_stmt.expr, tracker, scope).unwrap();
        }
        None => return Err(RuleSetGenError::NoTypeAnnotationError),
    }
    Ok(())
}

pub(super) fn generate_expr(expr: &Expr, tracker: &ValueTracker, scope: &Scope) -> IResult<()> {
    match &*expr.kind {
        ExprKind::If(if_expr) => {
            generate_if(if_expr, tracker).unwrap();
        }
        ExprKind::For(for_expr) => generate_for(for_expr, tracker).unwrap(),
        ExprKind::Match(match_expr) => {
            generate_match(match_expr, tracker).unwrap();
        }
        ExprKind::Return(return_expr) => {
            generate_return(return_expr, tracker).unwrap();
        }
        ExprKind::Send(send_expr) => {
            generate_send(send_expr, tracker).unwrap();
        }
        ExprKind::FnCall(fn_call_expr) => generate_fn_call(fn_call_expr, tracker).unwrap(),
        ExprKind::RuleCall(rule_call_expr) => generate_rule_call(rule_call_expr, tracker).unwrap(),
        ExprKind::Comp(comp_expr) => generate_comp(comp_expr, tracker, scope).unwrap(),
        ExprKind::Term(term_expr) => generate_term(term_expr, tracker).unwrap(),
        ExprKind::VariableCallExpr(variable_call_expr) => {
            generate_variable_call(variable_call_expr, tracker).unwrap()
        }
        ExprKind::Lit(lit_expr) => {
            generate_lit(&lit_expr, tracker).unwrap();
        }
        ExprKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    }
    Ok(())
}

pub(super) fn generate_if(if_expr: &If, tracker: &ValueTracker) -> IResult<()> {
    // if (block) {expression}
    // // (block)
    // let first_condition = generate_expr(
    //     &mut *if_expr.block,
    //     tracker
    // )
    // .unwrap();
    // let generated_stmt = {
    //     let mut gen_stmt = vec![];
    //     for stmt in &mut if_expr.stmts {
    //         gen_stmt.push(generate_stmt(stmt, tracker).unwrap());
    //     }
    //     gen_stmt
    // };

    // let generated_els = match &mut *if_expr.els {
    //     Some(els) => {
    //         let els_stmt = generate_stmt(els, tracker).unwrap();
    //     }
    //     None => quote!(),
    // };

    // let generated_elifs = {
    //     let mut elifs = vec![];
    //     for elif_expr in &mut *if_expr.elif {
    //         match &mut *elif_expr {
    //             Some(inner_if_expr) => elifs
    //                 .push(generate_if(inner_if_expr, ident_tracker, do_await, in_static).unwrap()),
    //             None => unreachable!(),
    //         }
    //     }
    //     elifs
    // };
    Ok(())
}

pub(super) fn generate_for(for_expr: &For, tracker: &ValueTracker) -> IResult<()> {
    Ok(())
}

pub(super) fn generate_fn_call(fn_call_expr: &FnCall, tracker: &ValueTracker) -> IResult<()> {
    // Before generating functions, check function table to check whether it's properly defined or not
    Ok(())
}

pub(super) fn generate_fn_call_arg(
    fn_call_arg: &FnCallArgs,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<ArgVals> {
    Ok(ArgVals::Str(String::from("")))
}

pub(super) fn generate_rule_call(rule_call_expr: &RuleCall, tracker: &ValueTracker) -> IResult<()> {
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
    let repeater_arg = &tracker.borrow().repeaters[repeater_index];
    let mut arguments = Arguments::place_holder();
    arguments.update_scope(scope);
    for (i, arg) in rule_call_expr.argument.iter().enumerate() {
        let arg_pos = format!("#{}", i);
        arguments.add_val(
            &arg_pos,
            generate_fn_call_arg(arg, tracker, scope).unwrap().clone(),
        );
    }

    // Check argument types, argument numbers, values
    // let stage = rule_evaluator(rule_name, repeater_arg, &arguments, tracker).unwrap();
    let stage = tracker
        .borrow()
        .eval_rule(rule_name, repeater_arg, &arguments);

    // Add stage to the ruleset
    // At this moment, repeater index must be corresponding to where this rule is added
    tracker.borrow().add_stage(repeater_index, stage);

    // Add rule to stage inside the RuleSet
    Ok(())
}

// Repeater call must be casted to usize to
pub(super) fn generate_rep_call_arg(
    repeater_call_arg: &RepeaterCallArg,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<usize> {
    let index: usize = match repeater_call_arg {
        RepeaterCallArg::Term(term) => 0,
        RepeaterCallArg::Ident(ident) => 0,
        RepeaterCallArg::NumberLit(number_lit) => {
            let val = &*number_lit.value.name.clone();
            val.parse::<usize>().unwrap()
        }
        RepeaterCallArg::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    };
    Ok(index as usize)
}

// Promote resources to the next stage
pub(super) fn generate_return(return_expr: &Return, tracker: &ValueTracker) -> IResult<()> {
    Ok(())
}

pub(super) fn generate_send(send_expr: &Send, tracker: &ValueTracker) -> IResult<()> {
    Ok(())
}

// Generate Match expression to achieve match action
// In the case of static generation, this expand Rules to the stages
pub(super) fn generate_match(match_expr: &Match, tracker: &ValueTracker) -> IResult<()> {
    Ok(())
}

pub(super) fn generate_match_arm(match_arm: &MatchArm, tracker: &ValueTracker) -> IResult<()> {
    // let generated_match_condition =
    //     generate_match_condition(&mut *match_arm.condition, ident_tracker).unwrap();
    // let generated_match_action =
    //     generate_match_action(&mut *match_arm.action, ident_tracker, false, false).unwrap();
    // Ok(quote!(#generated_match_condition => {#generated_match_action}))
    Ok(())
}

pub(super) fn generate_match_condition(
    match_condition: &MatchCondition,
    tracker: &ValueTracker,
) -> IResult<()> {
    // Right now, satisfiable can only take literals, but in the future, this should be more flexible
    match &*match_condition.satisfiable {
        Satisfiable::Lit(literal) => Ok(generate_lit(literal, tracker).unwrap()),
        _ => Err(RuleSetGenError::InitializationError),
    }
}

pub(super) fn generate_match_action(
    match_action: &MatchAction,
    ident_tracker: &ValueTracker,
) -> IResult<()> {
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
    Ok(())
}

pub(super) fn generate_comp(
    comp_expr: &Comp,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<()> {
    let lhs = generate_expr(&comp_expr.lhs, tracker, scope).unwrap();
    let rhs = generate_expr(&comp_expr.rhs, tracker, scope).unwrap();
    let (op, cmp_op) = match *comp_expr.comp_op {
        CompOpKind::Lt => (quote!(<), quote!(__CmpOp::Lt)),
        CompOpKind::Gt => (quote!(>), quote!(__CmpOp::Gt)),
        CompOpKind::LtE => (quote!(<=), quote!(__CmpOp::LtE)),
        CompOpKind::GtE => (quote!(>=), quote!(__CmpOp::GtE)),
        CompOpKind::Eq => (quote!(==), quote!(__CmpOp::Eq)),
        CompOpKind::Nq => (quote!(!=), quote!(__CmpOp::Nq)),
        CompOpKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    };
    Ok(())
}

// pub(super) fn generate_awaitable(
//     awaitable_expr: &Awaitable,
//     tracker: &ValueTracker,
// ) -> IResult<()> {
//     Ok(())
//     // Condition clauses should be awaited to be met
// // Check if the rule name is properly set
// let (in_rule, _) = helper::check_in(rule_name);
// if !in_rule {
//     return Err(RuLaCompileError::NoRuleFoundError);
// }

// let do_await = if in_static { false } else { true };
// let awaitable = match awaitable_expr {
//     Awaitable::FnCall(fn_call) => {
//         // This should be flex
//         let generated_fn_call = generate_fn_call(
//             fn_call,
//             rule_name,
//             ident_tracker,
//             do_await,
//             in_static,
//             false,
//         )
//         .unwrap();
//         quote!(
//             #generated_fn_call
//         )
// //     }
//     Awaitable::VariableCallExpr(val_call) => {
//         // Check if the value is properly watched or not
//         let generated_val = generate_variable_call(
//             val_call,
//             rule_name,
//             ident_tracker,
//             do_await,
//             in_static,
//             false,
//         )
//         .unwrap();
//         quote!(#generated_val)
//     }
//     Awaitable::Comp(comp) => {
//         let generated_comp = if in_static {
//             generate_comp(comp, rule_name, ident_tracker, false, in_static, false).unwrap()
//         } else {
//             generate_comp(comp, rule_name, ident_tracker, true, in_static, false).unwrap()
//         };
//         quote!(#generated_comp)
//     }
// };
// if in_static {
//     Ok(quote!(
//         let _ = #awaitable;
//     ))
// } else {
//     Ok(quote!(
//         #awaitable
//     ))
// }
// }

pub(super) fn generate_variable_call(
    variable_call_expr: &VariableCallExpr,
    tracker: &ValueTracker,
) -> IResult<()> {
    Ok(())
}

pub(super) fn generate_lit(lit: &Lit, tracker: &ValueTracker) -> IResult<()> {
    match &*lit.kind {
        LitKind::Ident(ident_lit) => generate_ident(ident_lit, tracker).unwrap(),
        LitKind::NumberLit(number_lit) => {
            LitFloat::new(&*number_lit.value.name, Span::call_site());
        }
        LitKind::StringLit(string_lit) => {
            SynLit::Str(LitStr::new(&string_lit.as_string(), Span::call_site()));
        }
        _ => todo!("{:#?} not yet implemented", lit),
    }
    Ok(())
}

pub(super) fn generate_term(term_expr: &Term, tracker: &ValueTracker) -> IResult<()> {
    // We could make Term struct instead of direct calc
    // For now, this function just returns calc result as f64
    // let val = LitFloat::new(&term_expr.to_string(), Span::call_site());
    // Ok(quote!(#val))
    Ok(())
}

// Generate identifier token from Ident ast
pub(super) fn generate_ident(ident: &Ident, tracker: &ValueTracker) -> IResult<()> {
    Ok(())
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
