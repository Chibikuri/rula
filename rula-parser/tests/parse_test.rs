extern crate rula_parser;

use rula_parser::parser::ast::*;
use std::path::PathBuf;

fn build_program_ast(program: Program) -> AstNode {
    AstNode::RuLa(RuLa::new(RuLaKind::Program(program)))
}

mod import_tests {
    use super::*;
    #[test]
    fn test_single_import() {
        let import_stmt = "import hello";
        let ast_nodes = rula_parser::parse(import_stmt).unwrap();
        let expected_paths = vec![["hello"].iter().collect()];
        let expected_ast_nodes = build_program_ast(Program::new(vec![ProgramKind::Import(
            Import::new(expected_paths, false, vec![], vec![]),
        )]));
        assert_eq!(ast_nodes, expected_ast_nodes);
    }

    #[test]
    fn test_nested_import() {
        let import_stmt = "import hello::world";
        let ast_nodes = rula_parser::parse(import_stmt).unwrap();
        let expected_paths = vec![["hello", "world"].iter().collect()];
        let expected_ast_nodes = build_program_ast(Program::new(vec![ProgramKind::Import(
            Import::new(expected_paths, false, vec![], vec![]),
        )]));
        assert_eq!(ast_nodes, expected_ast_nodes);
    }

    #[test]
    fn test_multiple_import() {
        let import_stmt = "import hello::{world, there}";
        let ast_nodes = rula_parser::parse(import_stmt).unwrap();

        let expected_path_hello_world = ["hello", "world"].iter().collect();
        let expected_path_hello_there = ["hello", "there"].iter().collect();

        let expected_paths = vec![expected_path_hello_world, expected_path_hello_there];
        let expected_ast_nodes = build_program_ast(Program::new(vec![ProgramKind::Import(
            Import::new(expected_paths, false, vec![], vec![]),
        )]));
        assert_eq!(ast_nodes, expected_ast_nodes);
    }

    #[test]
    fn test_rule_import() {
        let import_rule_stmt = "import (rule) examples::swapping::swapping";
        let ast_nodes = rula_parser::parse(import_rule_stmt).unwrap();

        // let expected_path_rule = ["swapping", "swap"].iter().collect();

        let expected_ast_nodes =
            build_program_ast(Program::new(vec![ProgramKind::Import(Import::new(
                vec![PathBuf::from("examples/swapping/swapping")],
                true,
                vec![RuleExpr::new(
                    Ident::new("swapping", None),
                    Ident::new("rep", None),
                    vec![],
                    None,
                    RuleContentExpr::new(
                        vec![],
                        CondExpr::new(None, vec![]),
                        ActExpr::new(None, vec![]),
                        vec![],
                    ),
                )],
                vec!["swapping".to_string()],
            ))]));
        assert_eq!(ast_nodes, expected_ast_nodes);
    }

    #[test]
    fn test_multiple_rule_import() {
        let import_rule_stmt = "import (rule) examples::pur::{parity_check, local_operation}";
        let ast_nodes = rula_parser::parse(import_rule_stmt).unwrap();

        let expected_ast_nodes =
            build_program_ast(Program::new(vec![ProgramKind::Import(Import::new(
                vec![
                    PathBuf::from("examples/pur/parity_check"),
                    PathBuf::from("examples/pur/local_operation"),
                ],
                true,
                vec![
                    RuleExpr::new(
                        Ident::new("parity_check", None),
                        Ident::new("rep", None),
                        vec![],
                        None,
                        RuleContentExpr::new(
                            vec![],
                            CondExpr::new(None, vec![]),
                            ActExpr::new(None, vec![]),
                            vec![],
                        ),
                    ),
                    RuleExpr::new(
                        Ident::new("local_operation", None),
                        Ident::new("rep", None),
                        vec![],
                        None,
                        RuleContentExpr::new(
                            vec![],
                            CondExpr::new(None, vec![]),
                            ActExpr::new(None, vec![]),
                            vec![],
                        ),
                    ),
                ],
                vec!["parity_check".to_string(), "local_operation".to_string()],
            ))]));
        assert_eq!(ast_nodes, expected_ast_nodes);
    }

    #[test]
    fn test_import_syntax_error_imcomplete_path() {
        // Error test
        // imcompelete path
        let import_stmt = "import hello::;";
        let error_ast_nodes = rula_parser::parse(import_stmt);
        assert!(error_ast_nodes.is_err());
    }

    #[test]
    fn test_import_syntax_error_imcomplete_path_multiple() {
        // imcomplete error
        let import_stmt = "import hello::{world, };";
        let error_ast_nodes = rula_parser::parse(import_stmt);
        assert!(error_ast_nodes.is_err());
    }
}

mod comment_tests {
    use super::*;
    #[test]
    #[rustfmt::skip]
    fn test_implicit_parse_comment() {
        let comment = "// this is a comment \n";
        let empty_ast_nodes = rula_parser::parse(comment).unwrap();
        let target_ast_nodes = 
            AstNode::RuLa(
                RuLa::new(
                    RuLaKind::Ignore
                )
            );
        assert_eq!(empty_ast_nodes, target_ast_nodes);
    }

    #[test]
    #[rustfmt::skip]
    fn test_parse_long_comment() {
        let comment = "/* this is a comment */";
        let empty_ast_nodes = rula_parser::parse(comment).unwrap();
        let target_ast_nodes =
            AstNode::RuLa(
                RuLa::new(
                    RuLaKind::Ignore
                )
            );
        assert_eq!(empty_ast_nodes, target_ast_nodes);
    }

    #[test]
    #[rustfmt::skip]
    fn test_parse_long_comment_with_new_line() {
        let comment = "/* this is a comment\n hello world\n */";
        let empty_ast_nodes = rula_parser::parse(comment).unwrap();
        let target_ast_nodes = 
            AstNode::RuLa(
                RuLa::new(
                    RuLaKind::Ignore
                )
            );
        assert_eq!(empty_ast_nodes, target_ast_nodes);
    }
}

mod test_ruleset_stmt {
    use super::*;

    #[test]
    fn test_simple_ruleset() {
        let ruleset_stmt = "ruleset entanglement_swapping{
            stmts
        }";
        let ruleset_stmt_asts = rula_parser::parse(ruleset_stmt).unwrap();

        let target_ast_nodes = build_program_ast(Program::new(vec![ProgramKind::RuleSetExpr(
            RuleSetExpr::new(
                Ident::new("entanglement_swapping", None),
                vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
                    Lit::new(LitKind::Ident(Ident::new("stmts", None))),
                ))))],
            ),
        )]));
        assert_eq!(ruleset_stmt_asts, target_ast_nodes);
    }
}

mod test_rule_stmt {
    use super::*;

    #[test]
    fn test_simple_rule_stmt() {
        let rule_stmt = "rule hello<#rep>(q2: Qubit, q3: Qubit) :-> Qubit? {cond{} => act{}}";
        let rule_stmt_asts = rula_parser::parse(rule_stmt).unwrap();
        let target_ast_nodes =
            build_program_ast(Program::new(vec![ProgramKind::RuleExpr(RuleExpr::new(
                Ident::new("hello", None),
                Ident::new("rep", None),
                vec![
                    Ident::new("q2", Some(TypeDef::Qubit)),
                    Ident::new("q3", Some(TypeDef::Qubit)),
                ],
                Some(ReturnTypeAnnotation::new(vec![(TypeDef::Qubit, true)])),
                RuleContentExpr::new(
                    vec![],
                    CondExpr::new(None, vec![]),
                    ActExpr::new(None, vec![]),
                    vec![],
                ),
            ))]));
        assert_eq!(target_ast_nodes, rule_stmt_asts);
    }
}
