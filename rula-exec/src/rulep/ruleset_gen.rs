use std::env;
use std::fs;
use std::io::Write;
use std::net::Ipv4Addr;
use std::process::Command;

use super::action::v2::Action as ActionV2;
use super::ruleset::RuleSet;
use super::IResult;
use rula_parser::parser::ast::*;

pub fn generate_ruleset(ruleset: &RuleSetExpr) -> IResult<RuleSet<ActionV2>> {
    // generate ruleset from rule_expr
    let ruleset_name = &*ruleset.name.name;
    let default_rule = match &*ruleset.default {
        Some(default_rule) => default_rule,
        None => todo!("No default rule!"),
    };
    let rules = &*ruleset.rules;
    for stmt in rules {
        // statement
        match &*stmt.kind {
            // get return value from rule
            StmtKind::Let(let_stmt) => {
                // This info might not be used at this moment
                let return_val_store = &*let_stmt.ident;
                let rule_expr = match &*let_stmt.expr.kind {
                    ExprKind::FnCall(rule_name) => &*rule_name.func_name,
                    _ => todo!("error"),
                };
            }
            StmtKind::Expr(expr) => {}
            _ => todo!("Here ruleset suppose to be a set of rules."),
        }
    }
    // let rule_table = vec![];

    let ruleset = RuleSet::new(ruleset_name);
    Ok(ruleset)
}

// Helper function to generate
pub fn generate_ruleset_file<T>(program: RuleSet<T>, file_name: &str) {
    let mut file_path = env::current_dir().unwrap();
    file_path.push("tests");
    file_path.push("generated");
    file_path.push(file_name);
    let mut file = fs::File::create(file_path.clone()).unwrap();
    // writeln!(file, "// This is autogenerated Rust program \n{}", program).unwrap();
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
        let test_ruleset = RuleSetExpr::new(
            Ident::new("entanglement_swapping", None),
            Some(FnCall::new(Ident::new("default", None), vec![])),
            vec![
                Stmt::new(StmtKind::Let(Let::new(
                    Ident::new("q1_entangled", None),
                    Expr::new(ExprKind::FnCall(FnCall::new(
                        Ident::new("swapping", None),
                        vec![],
                    ))),
                ))),
                Stmt::new(StmtKind::Expr(Expr::new(ExprKind::FnCall(FnCall::new(
                    Ident::new("pauli_correction", None),
                    vec![Ident::new("q1_entangled", None)],
                ))))),
            ],
        );

        let mut target_ruleset = RuleSet::<ActionV2>::new("entanglement_swapping");
        target_ruleset.add_rule(Rule::<ActionV2>::new("swappping"));
        target_ruleset.add_rule(Rule::<ActionV2>::new("pauli_correction"));
        let generated_ruleset = generate_ruleset(&test_ruleset).unwrap();
        // assert_eq!(target_ruleset, generated_ruleset);
        // assert_eq!(&generated_ruleset.rules[0].name, "swapping");
        // assert_eq!(&generated_ruleset.rules[1].name, "pauli_correction");
    }
}
