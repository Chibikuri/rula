#[cfg(test)]
mod tests {
    use super::super::ruleset_generator::*;
    use super::super::tracker::Tracker;
    use super::super::types::Types;
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
                vec![Ident::new("x", Some(TypeDef::Integer))],
                Expr::new(ExprKind::Lit(Lit::new(LitKind::NumberLit(NumberLit::new(
                    NumberLitKind::IntegerLit(IntegerLit::new("19", false)),
                ))))),
            );
            let generated_let =
                generate_let(&target_ast, &mock_tracker(), &String::from("Test"), true)
                    .unwrap()
                    .to_string();
            let expected = "let x : i64 = 19 ;";
            assert_eq!(generated_let, expected);
        }

        #[test]
        fn test_multi_arg_let() {
            // (original) let (x: qubit, y: qubit) = generate();
            // (generated) let (x, y): (qubit, qubit) = generated();
            let target_ast = Let::new(
                vec![
                    Ident::new("x", Some(TypeDef::Qubit)),
                    Ident::new("y", Some(TypeDef::Qubit)),
                ],
                Expr::new(ExprKind::FnCall(FnCall::new(
                    Ident::new("generated", None),
                    false,
                    vec![],
                ))),
            );
            let generated_let =
                generate_let(&target_ast, &mock_tracker(), &String::from("Test"), true)
                    .unwrap()
                    .to_string();
            let expected = "let (x , y) : (Qubit , Qubit) = generated (Rc :: clone (& rules) ,) ;";
            assert_eq!(generated_let, expected);
        }

        #[test]
        fn tracker_register_test() {
            // (original) let test: int = 20;

            let tracker = mock_tracker();
            let target_ast = Let::new(
                vec![Ident::new("test", Some(TypeDef::Integer))],
                Expr::new(ExprKind::Lit(Lit::new(LitKind::NumberLit(NumberLit::new(
                    NumberLitKind::IntegerLit(IntegerLit::new("19", false)),
                ))))),
            );
            let _ = generate_let(&target_ast, &tracker, &String::from("Test"), true);
            let variables = &tracker.borrow().variables;
            assert!(variables.contains_key("Test"));
            assert!(variables
                .get("Test")
                .expect("Failed to get scope")
                .contains_key("test"));
            assert_eq!(
                variables.get("Test").expect("").get("test").expect(""),
                &Types::Int
            );
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

            let target_ast = Import::new(
                vec![PathBuf::from("test/hollow")],
                false,
                vec![],
                vec![],
                vec![],
            );

            let generated_import = generate_import(&target_ast, &mock_tracker())
                .unwrap()
                .to_string();
            let expected = "use test :: hollow ;";
            assert_eq!(generated_import, expected);
        }
    }

    #[cfg(test)]
    mod term_expr_tests {

        #[test]
        fn simple_term_expr() {
            // (original) x/2 + 1
        }
    }
}
