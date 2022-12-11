// This is entory point to generate code from AST
use super::conf_parser::parse_config;
use super::error::RuleSetGenError;
use super::ruleset::RuleSet;
use super::tracker::Tracker;
use super::IResult;

use rula_parser::parser::ast::*;

use std::collections::HashMap;
use std::path::PathBuf;

use proc_macro2::{Span, TokenStream};
use syn::Lit as SynLit;
use syn::{LitFloat, LitStr};

/// Generate corresponding rust code from ast
/// Every nested generators returns a piece of TokenStream
/// Arguments:
///     ast_tree
pub fn generate(ast_tree: &AstNode, config_path: PathBuf) -> IResult<HashMap<u32, RuleSet>> {
    // RuLa should know how many repeaters inconfig at this moment
    // TODO: Just mock here
    let num_nodes = parse_config(config_path);

    // Initialize tracker to track global state over the functions
    let mut tracker = Tracker::new();
    // Add empty RuleSets
    for i in 0..num_nodes {
        tracker.add_ruleset(i, RuleSet::new("empty"));
    }
    // Generated rula program
    match ast_tree {
        // All RuLa AST starts RuLa Node
        AstNode::RuLa(rula) => generate_rula(rula, &mut tracker).unwrap(),
        AstNode::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    };
    // return generated rulesets
    Ok(tracker.rulesets.clone())
}

// Generate entire rula program
pub(super) fn generate_rula(rula: &RuLa, tracker: &mut Tracker) -> IResult<()> {
    match &*rula.rula {
        RuLaKind::Program(program) => {
            generate_program(program, tracker);
            Ok(())
        }
        RuLaKind::Ignore => Ok(()),
        RuLaKind::Eoi => Ok(()),
        RuLaKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    }
}

// Generate progra
// program: Program AST that contains a vector of Stmt
pub(super) fn generate_program(program: &Program, tracker: &mut Tracker) -> IResult<()> {
    // A vector to store a set of generated stmts
    for program_kind in &program.programs {
        // Right now, a program can takes a stmt
        match program_kind {
            ProgramKind::Repeaters => {
                todo!()
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

pub(super) fn generate_import(import_expr: &Import, tracker: &mut Tracker) -> IResult<()> {
    // How to get the other rules?
    Ok(())
}

pub(super) fn generate_ruleset_expr(
    ruleset_expr: &RuleSetExpr,
    racker: &mut Tracker,
) -> IResult<()> {
    // 1. Generate static RuleSet for serialized output
    // Get meta information for this RuleSet
    let ruleset_name = &*ruleset_expr.name.name;
    Ok(())
}

pub(super) fn generate_rule_expr(rule_expr: &RuleExpr, ident_tracker: &mut Tracker) -> IResult<()> {
    Ok(())
}

// Generate stmt expression
// Arguments:
//  stmt: Stmt AST
//  rule_name: Option<String> a name of corresponding Rule
//  ident_tracker: List of identifiers that need to be tracked
pub(super) fn generate_stmt(
    stmt: &Stmt,
    rule_name: Option<&String>,
    tracker: &mut Tracker,
) -> IResult<()> {
    match &*stmt.kind {
        StmtKind::Let(let_stmt) => {
            generate_let(let_stmt, tracker).unwrap();
        }
        StmtKind::Expr(expr) => {
            generate_expr(expr, tracker).unwrap();
        }
        StmtKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    }
    Ok(())
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
pub(super) fn generate_let(let_stmt: &Let, ident_tracker: &mut Tracker) -> IResult<()> {
    // If this is watched value, register value to the identifier tracker
    Ok(())
}

pub(super) fn generate_expr(expr: &Expr, tracker: &mut Tracker) -> IResult<()> {
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
        ExprKind::Comp(comp_expr) => generate_comp(comp_expr, tracker).unwrap(),
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

pub(super) fn generate_if(if_expr: &If, tracker: &mut Tracker) -> IResult<()> {
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

pub(super) fn generate_for(for_expr: &For, tracker: &mut Tracker) -> IResult<()> {
    Ok(())
}

pub(super) fn generate_fn_call(fn_call_expr: &FnCall, tracker: &mut Tracker) -> IResult<()> {
    // Before generating functions, check function table to check whether it's properly defined or not
    Ok(())
}

pub(super) fn generate_rule_call(rule_call_expr: &RuleCall, tracker: &mut Tracker) -> IResult<()> {
    Ok(())
}

// Promote resources to the next stage
pub(super) fn generate_return(return_expr: &Return, tracker: &mut Tracker) -> IResult<()> {
    Ok(())
}

pub(super) fn generate_send(send_expr: &Send, tracker: &mut Tracker) -> IResult<()> {
    Ok(())
}

// Generate Match expression to achieve match action
// In the case of static generation, this expand Rules to the stages
pub(super) fn generate_match(match_expr: &Match, tracker: &mut Tracker) -> IResult<()> {
    Ok(())
}

pub(super) fn generate_match_arm(match_arm: &MatchArm, tracker: &mut Tracker) -> IResult<()> {
    // let generated_match_condition =
    //     generate_match_condition(&mut *match_arm.condition, ident_tracker).unwrap();
    // let generated_match_action =
    //     generate_match_action(&mut *match_arm.action, ident_tracker, false, false).unwrap();
    // Ok(quote!(#generated_match_condition => {#generated_match_action}))
    Ok(())
}

pub(super) fn generate_match_condition(
    match_condition: &MatchCondition,
    tracker: &mut Tracker,
) -> IResult<()> {
    // Right now, satisfiable can only take literals, but in the future, this should be more flexible
    match &*match_condition.satisfiable {
        Satisfiable::Lit(literal) => Ok(generate_lit(literal, tracker).unwrap()),
        _ => Err(RuleSetGenError::InitializationError),
    }
}

pub(super) fn generate_match_action(
    match_action: &MatchAction,
    ident_tracker: &mut Tracker,
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

pub(super) fn generate_comp(comp_expr: &Comp, tracker: &mut Tracker) -> IResult<()> {
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
    Ok(())
}

pub(super) fn generate_rule_content(
    rule_content_expr: &RuleContentExpr,
    tracker: &mut Tracker,
) -> IResult<()> {
    Ok(())
    //     // Actions to be performed
    //     let generated_act = generate_act(
    //         &mut *rule_content_expr.action_expr,
    //         tracker,
    //     )
    //     .unwrap();

    //     let generated_cond = generate_cond(
    //         &mut *rule_content_expr.condition_expr,
    //         tracker,
    //     )
    //     .unwrap();

    //     let post_process = &mut rule_content_expr.post_processing;
    //     let mut post_processes = vec![];
    //     for stmt in post_process {
    //         post_processes.push(generate_stmt(stmt, None, ident_tracker, false, false).unwrap());
    //     }
    //     // Generate executable
    //     // Here we should consider to use TaskMonitor in tokio to track the status of watched values
    //     Ok(quote!(
    //         async fn condition(&self) -> bool{
    //             let interface = INTERFACES.get().expect("Unable to get interface table").lock().await;
    //             #generated_cond
    //         }

    //         fn post_process(&self){
    //             #(#post_processes)*
    //         }
    //         // pub is implied in trait
    //         async fn execute(&self){
    //             loop{
    //                 let __done = self.condition().await;
    //                 if __done{
    //                     break;
    //                 }
    //                 sleep(Duration::from_micros(100));
    //             }
    //         }
    //     ))
}

pub(super) fn generate_cond(cond_expr: &CondExpr, tracker: &mut Tracker) -> IResult<()> {
    // let (in_rule, r_name) = helper::check_in(rule_name);

    // if !in_rule {
    //     return Err(RuLaCompileError::NoRuleFoundError);
    // }

    // // Generate condition clauses
    // let generated_clauses = {
    //     let mut clauses = vec![];
    //     for clause in &mut cond_expr.clauses {
    //         clauses.push(generate_awaitable(clause, Some(&r_name), ident_tracker, false).unwrap());
    //     }
    //     clauses
    // };

    // // If there is a watch expression, generate watch expression
    // let generated_watch = match &mut *cond_expr.watch_expr {
    //     Some(watch_expr) => {
    //         generate_watch(watch_expr, Some(&r_name), ident_tracker, false).unwrap()
    //     }
    //     None => quote!(),
    // };

    // Ok(quote!(
    //     #generated_watch
    //     if #(#generated_clauses)&&*{
    //     // self.action().await;
    //     (||async {
    //         #act_tokens
    //     })().await;
    //     return true
    //     }else{
    //     return false
    //     };
    // ))
    Ok(())
}

// pub(super) fn generate_awaitable(
//     awaitable_expr: &Awaitable,
//     tracker: &mut Tracker,
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

pub(super) fn generate_act(act_expr: &mut ActExpr, tracker: &mut Tracker) -> IResult<()> {
    // let mut action_calls = vec![];

    // for action in &mut act_expr.operatable {
    //     // TODO: Should integrate but for now, we need to know if there is ";" at the end
    //     match &mut *action.kind {
    //         StmtKind::Let(let_stmt) => action_calls
    //             .push(generate_let(let_stmt, rule_name, ident_tracker, false, false).unwrap()),
    //         _ => {
    //             let generated =
    //                 generate_stmt(action, rule_name, ident_tracker, true, false).unwrap();
    //             action_calls.push(quote!(
    //                 #generated;
    //             ))
    //         }
    //     }
    // }
    // Ok(quote!(
    //     #(#action_calls)*
    // ))
    Ok(())
}

pub(super) fn generate_variable_call(
    variable_call_expr: &VariableCallExpr,
    tracker: &mut Tracker,
) -> IResult<()> {
    Ok(())
}

pub(super) fn generate_lit(lit: &Lit, tracker: &mut Tracker) -> IResult<()> {
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

pub(super) fn generate_term(term_expr: &Term, tracker: &mut Tracker) -> IResult<()> {
    // We could make Term struct instead of direct calc
    // For now, this function just returns calc result as f64
    // let val = LitFloat::new(&term_expr.to_string(), Span::call_site());
    // Ok(quote!(#val))
    Ok(())
}

// Generate identifier token from Ident ast
pub(super) fn generate_ident(ident: &Ident, tracker: &mut Tracker) -> IResult<()> {
    Ok(())
}

pub(super) fn generate_type_hint(type_hint: &TypeDef) -> IResult<TokenStream> {
    match type_hint {
        TypeDef::Boolean => Ok(quote!(bool)),
        TypeDef::Integer => Ok(quote!(i64)),
        TypeDef::UnsignedInteger => Ok(quote!(u64)),
        TypeDef::Float => Ok(quote!(f64)),
        TypeDef::Str => Ok(quote!(String)),
        TypeDef::Qubit => Ok(quote!(QubitInterface)),
        TypeDef::Vector(inner) => {
            let inner_type = generate_type_hint(inner).unwrap();
            Ok(quote!(Vec<#inner_type>))
        }
        TypeDef::PlaceHolder => return Err(RuleSetGenError::InitializationError),
        _ => todo!("type {:#?} not yet implemented", type_hint),
    }
}
