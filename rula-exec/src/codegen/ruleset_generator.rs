use std::collections::HashMap;
use std::sync::Mutex;

use super::rule_meta::*;
use super::IResult;

use crate::rulep::action::v2::ActionClauses;
use crate::rulep::action::Action;
use crate::rulep::ruleset::RuleSet;

use rula_parser::parser::ast::*;

use once_cell::sync::OnceCell;

static RULE_META: OnceCell<Mutex<HashMap<String, RuleMeta>>> = OnceCell::new();

// Trait that tells translation from expressions to Rule isntructions
pub trait RuleSetTranslatable {}

pub fn generate_ruleset(ruleset: &RuleSetExpr) -> IResult<RuleSet<Action<ActionClauses>>> {
    // generate ruleset from rule_expr
    let ruleset_name = &*ruleset.name.name;
    let _default_rule = match &*ruleset.default {
        Some(default_rule) => default_rule,
        None => todo!("No default rule!"),
    };
    let rules = &*ruleset.rules;
    for _stmt in rules {
        // statement
        // match &*stmt.kind {
        //     // get return value from rule
        //     StmtKind::Let(let_stmt) => {
        //         // This info might not be used at this moment
        //         let return_val_store = &*let_stmt.ident;
        //         let rule_expr = match &*let_stmt.expr.kind {
        //             ExprKind::FnCall(rule_name) => &*rule_name.func_name,
        //             _ => todo!("error"),
        //         };
        //     }
        //     StmtKind::Expr(expr) => {}
        //     _ => todo!("Here ruleset suppose to be a set of rules."),
        // }
    }
    // let rule_table = vec![];

    let ruleset = RuleSet::new(ruleset_name);
    Ok(ruleset)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rulep::ruleset::Rule;

    #[test]
    fn test_make_json() {
        // let ruleset = generate_ruleset();
    }

    #[test]
    fn test_generate_simple_ruleset() {
        // ruleset entanglement_swapping{
        //     default: default()
        //     let q1_entangled = swapping();
        //     pauli_correction(q1_entangled)
        // }
        // let test_ruleset = RuleSetExpr::new(
        //     Ident::new("entanglement_swapping", None),
        //     None,
        //     Some(FnCall::new(Ident::new("default", None), vec![])),
        //     vec![
        //         RuleIdentifier::Let(Let::new(
        //             Ident::new("q1_entangled", None),
        //             Expr::new(ExprKind::FnCall(FnCall::new(
        //                 Ident::new("swapping", None),
        //                 vec![],
        //             ))),
        //         )),
        //         RuleIdentifier::FnCall(FnCall::new(
        //             Ident::new("pauli_correction", None),
        //             vec![Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(
        //                 Ident::new("q1_entangled", None),
        //             ))))],
        //         )),
        //     ],
        // );

        // let mut target_ruleset = RuleSet::<Action<ActionClauses>>::new("entanglement_swapping");
        // target_ruleset.add_rule(Rule::<Action<ActionClauses>>::new("swappping"));
        // target_ruleset.add_rule(Rule::<Action<ActionClauses>>::new("pauli_correction"));
        // let _generated_ruleset = generate_ruleset(&test_ruleset).unwrap();
        // assert_eq!(target_ruleset, generated_ruleset);
        // assert_eq!(&generated_ruleset.rules[0].name, "swapping");
        // assert_eq!(&generated_ruleset.rules[1].name, "pauli_correction");
    }
}
