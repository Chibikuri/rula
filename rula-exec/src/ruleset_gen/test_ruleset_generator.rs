#[cfg(test)]
mod tests {
    use super::super::ruleset_generator::*;
    use super::super::tracker::Tracker;
    use rula_parser::parser::ast::*;
    use std::cell::RefCell;

    fn mock_tracker() -> ValueTracker {
        RefCell::new(Tracker::new())
    }

    #[cfg(test)]
    mod let_stmt_tests {
        use super::*;

        #[test]
        fn simple_let_gen() {
            // (original) let x: int = 19;
            // (generated) let x: i64 = RuLaValue::Int(19 as i64);

            let target_ast = Let::new(
                Ident::new("x", Some(TypeDef::Integer)),
                Expr::new(ExprKind::Lit(Lit::new(LitKind::NumberLit(NumberLit::new(
                    NumberLitKind::IntegerLit(IntegerLit::new("19", false)),
                ))))),
            );
            let generated_let =
                generate_let(&target_ast, &mock_tracker(), &String::from("Test"), false)
                    .unwrap()
                    .to_string();
            let expected = "let x : i64 = 19 ;";
            assert_eq!(generated_let, expected);
        }
    }

    #[cfg(test)]
    mod import_tests {
        use super::*;
        use std::path::PathBuf;

        #[test]
        fn simple_import() {
            // (original) import test::hollow;
            // (generated) use test::hollow;

            let target_ast = Import::new(PathKind::from(vec![PathBuf::from("test/hollow")]));

            let generated_import = generate_import(&target_ast, &mock_tracker())
                .unwrap()
                .to_string();
            let expected = "use test :: hollow ;";
            assert_eq!(generated_import, expected);
        }
    }

    #[cfg(test)]
    mod term_expr_tests {
        use super::*;

        #[test]
        fn simple_term_expr() {
            // (original) x/2 + 1
        }
    }
}
