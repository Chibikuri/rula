extern crate rula;

use rula::parser::ast::{
    AstNode, Expr, ExprKind, Ident, If, Import, Let, Lit, LitKind, PathKind, Program, ProgramKind,
    RuLa, RuLaKind, Stmt, StmtKind, StringLit,
};

mod import_tests {
    use super::*;
    #[test]
    #[rustfmt::skip]
    fn test_single_import() {
        let import_expr = "import hello;";
        let ast_nodes = rula::parse(import_expr).unwrap();
        let expected_paths = vec![["hello"].iter().collect()];
        let expected_ast_nodes = vec![
            AstNode::RuLa(
                RuLa::new(
                    RuLaKind::Program(
                        Program::new(
                            ProgramKind::Stmt(
                                Stmt::new(StmtKind::Expr(Expr::new(
                                    ExprKind::Import(Import::new(PathKind::from(expected_paths))),
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    ];
        assert_eq!(ast_nodes.len(), 1);
        assert_eq!(ast_nodes[0], expected_ast_nodes[0]);
    }

    #[test]
    #[rustfmt::skip]
    fn test_nested_import() {
        let import_expr = "import hello::world;";
        let ast_nodes = rula::parse(import_expr).unwrap();
        let expected_paths = vec![["hello", "world"].iter().collect()];
        let expected_ast_nodes = vec![
            AstNode::RuLa(
                RuLa::new(
                    RuLaKind::Program(
                        Program::new(
                            ProgramKind::Stmt(
                                Stmt::new(
                                    StmtKind::Expr(
                                        Expr::new(
                                            ExprKind::Import(
                                                Import::new(
                                                    PathKind::from(expected_paths)
                                                )
                                            ),
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            ];
        assert_eq!(ast_nodes[0], expected_ast_nodes[0]);
    }

    #[test]
    #[rustfmt::skip]
    fn test_multiple_import() {
        let import_expr = "import hello::{world, there};";
        let ast_nodes = rula::parse(import_expr).unwrap();

        let expected_path_hello_world = ["hello", "world"].iter().collect();
        let expected_path_hello_there = ["hello", "there"].iter().collect();

        let expected_paths = vec![expected_path_hello_world, expected_path_hello_there];
        let expected_ast_nodes = vec![
            AstNode::RuLa(
                RuLa::new(
                    RuLaKind::Program(
                        Program::new(
                            ProgramKind::Stmt(
                                Stmt::new(
                                    StmtKind::Expr(
                                        Expr::new(
                                            ExprKind::Import(
                                                Import::new(
                                                    PathKind::from(
                                                        expected_paths
                                                    )
                                                )
                                            ),
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            ];
        assert_eq!(ast_nodes[0], expected_ast_nodes[0]);
    }

    #[test]
    #[rustfmt::skip]
    fn test_import_syntax_error_imcomplete_path() {
        // Error test
        // imcompelete path
        let import_expr = "import hello::;";
        let error_ast_nodes = rula::parse(import_expr);
        assert!(error_ast_nodes.is_err());
    }

    #[test]
    #[rustfmt::skip]
    fn test_import_syntax_error_imcomplete_path_multiple() {
        // imcomplete error
        let import_expr = "import hello::{world, };";
        let error_ast_nodes = rula::parse(import_expr);
        assert!(error_ast_nodes.is_err());
    }
}

mod comment_tests {
    use super::*;
    #[test]
    #[rustfmt::skip]
    fn test_implicit_parse_comment() {
        let comment = "// this is a comment \n";
        let empty_ast_nodes = rula::parse(comment).unwrap();
        let target_ast_nodes = vec![
            AstNode::RuLa(
                RuLa::new(
                    RuLaKind::Ignore
                )
            )
            ];
        assert_eq!(empty_ast_nodes.len(), 1);
        assert_eq!(empty_ast_nodes[0], target_ast_nodes[0]);
    }

    #[test]
    #[rustfmt::skip]
    fn test_parse_long_comment() {
        let comment = "/* this is a comment */";
        let empty_ast_nodes = rula::parse(comment).unwrap();
        let target_ast_nodes = vec![
            AstNode::RuLa(
                RuLa::new(
                    RuLaKind::Ignore
                )
            )
        ];
        assert_eq!(empty_ast_nodes.len(), 1);
        assert_eq!(empty_ast_nodes[0], target_ast_nodes[0]);
    }

    #[test]
    #[rustfmt::skip]
    fn test_parse_long_comment_with_new_line() {
        let comment = "/* this is a comment\n hello world\n */";
        let empty_ast_nodes = rula::parse(comment).unwrap();
        let target_ast_nodes = vec![
            AstNode::RuLa(
                RuLa::new(
                    RuLaKind::Ignore
                )
            )
        ];
        assert_eq!(empty_ast_nodes.len(), 1);
        assert_eq!(empty_ast_nodes[0], target_ast_nodes[0]);
    }
}

mod let_tests {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_simple_let_stmt() {
        let let_stmt = r#"let hello = "world";"#;
        let let_ast_nodes = rula::parse(let_stmt).unwrap();
        let target_ast_nodes = vec![
            AstNode::RuLa(
                RuLa::new(
                    RuLaKind::Program(
                        Program::new(
                            ProgramKind::Stmt(
                                Stmt::new(
                                    StmtKind::Let(
                                        Let::new(
                                            Ident::new("hello"),
                                            Expr::new(
                                                ExprKind::Lit(
                                                    Lit::new(
                                                        LitKind::StringLit(
                                                            StringLit::new(
                                                                "world"
                                                        )
                                                    )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            ];
        assert_eq!(let_ast_nodes, target_ast_nodes);
    }

    #[test]
    #[rustfmt::skip]
    fn test_let_with_if_expr(){
        let let_stmt = "let hello = if(block){expression;};";
        let let_if_ast_nodes = rula::parse(let_stmt).unwrap();
        let target_ast_nodes = vec![
            AstNode::RuLa(
                RuLa::new(
                    RuLaKind::Program(
                        Program::new(
                            ProgramKind::Stmt(
                                Stmt::new(
                                    StmtKind::Let(
                                        Let::new(
                                            Ident::new(
                                                "hello"
                                            ),
                                            Expr::new(
                                                ExprKind::If(
                                                    If::new(
                                                        // (block)
                                                        Expr::new(
                                                            ExprKind::Ident(
                                                                Ident::new(
                                                                    "block"
                                                                )
                                                            )
                                                        ),
                                                        // {expression}
                                                        Stmt::new(
                                                            StmtKind::Expr(
                                                                Expr::new(
                                                                    ExprKind::Ident(
                                                                        Ident::new(
                                                                            "expression"
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        ),
                                                        // elif ~
                                                        None,
                                                        // else ~
                                                        None,
                                                    )
                                                )
                                            )
                
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        ];
        assert_eq!(target_ast_nodes, let_if_ast_nodes)
    }
}

mod if_tests {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_single_if_expr() {
        let if_expr = "if(block){expression;};";
        let if_ast_nodes = rula::parse(if_expr).unwrap();
        let target_ast_nodes = vec![
            AstNode::RuLa(
                RuLa::new(
                    RuLaKind::Program(
                        Program::new(
                            ProgramKind::Stmt(
                                Stmt::new(
                                    StmtKind::Expr(
                                        Expr::new(
                                            ExprKind::If(
                                                If::new(
                                                    // (block)
                                                    Expr::new(
                                                        ExprKind::Ident(
                                                            Ident::new(
                                                                "block"
                                                            )
                                                        )
                                                    ),
                                                    // {expression}
                                                    Stmt::new(
                                                        StmtKind::Expr(
                                                            Expr::new(
                                                                ExprKind::Ident(
                                                                    Ident::new(
                                                                        "expression"
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    ),
                                                    // elif ~
                                                    None,
                                                    // else ~
                                                    None,
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        ];
        assert_eq!(target_ast_nodes, if_ast_nodes);
    }

    #[test]
    #[rustfmt::skip]
    fn test_if_else_expr() {
        let if_else = "if(block){expression;}else{expression2;};";
        let if_else_ast_nodes = rula::parse(if_else).unwrap();
        let target_ast_nodes = vec![
            AstNode::RuLa(
                RuLa::new(
                    RuLaKind::Program(
                        Program::new(
                            ProgramKind::Stmt(
                                Stmt::new(
                                    StmtKind::Expr(
                                        Expr::new(
                                            ExprKind::If(
                                                If::new(
                                                    // (block)
                                                    Expr::new(
                                                        ExprKind::Ident(
                                                            Ident::new(
                                                                "block"
                                                            )
                                                        )
                                                    ),
                                                    // {expression}
                                                    Stmt::new(
                                                        StmtKind::Expr(
                                                            Expr::new(
                                                                ExprKind::Ident(
                                                                    Ident::new(
                                                                        "expression"
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    ),
                                                    // elif ~
                                                    None,
                                                    // else ~
                                                    Some(
                                                        Stmt::new(
                                                            StmtKind::Expr(
                                                                Expr::new(
                                                                    ExprKind::Ident(
                                                                        Ident::new(
                                                                            "expression2"
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        ];
        assert_eq!(target_ast_nodes, if_else_ast_nodes);
    }

    #[test]
    #[rustfmt::skip]
    fn test_if_elif_expr() {
        let if_elif_expr = "if(block){expression;} else if (block2){expression2;};";
        let if_elif_ast_nodes = rula::parse(if_elif_expr).unwrap();
        let target_ast_nodes = vec![
            AstNode::RuLa(
                RuLa::new(
                    RuLaKind::Program(
                        Program::new(
                            ProgramKind::Stmt(
                                Stmt::new(
                                    StmtKind::Expr(
                                        Expr::new(
                                            ExprKind::If(
                                                If::new(
                                                    // (block)
                                                    Expr::new(
                                                        ExprKind::Ident(
                                                            Ident::new(
                                                                "block"
                                                            )
                                                        )
                                                    ),
                                                    // {expression}
                                                    Stmt::new(
                                                        StmtKind::Expr(
                                                            Expr::new(
                                                                ExprKind::Ident(
                                                                    Ident::new(
                                                                        "expression"
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    ),
                                                    // elif ~
                                                    Some(
                                                        If::new(
                                                            // else if (block)
                                                            Expr::new(
                                                                ExprKind::Ident(
                                                                    Ident::new(
                                                                        "block2"
                                                                    )
                                                                )
                                                            ),
                                                            // else if () {statement2;};
                                                            Stmt::new(
                                                                StmtKind::Expr(
                                                                    Expr::new(
                                                                        ExprKind::Ident(
                                                                            Ident::new(
                                                                                "expression2"
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            ),
                                                            None,
                                                            None,
                                                        ),
                                                    ),
                                                    // else ~
                                                    None,
                                                )
                                            )
                
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        ];
        assert_eq!(target_ast_nodes, if_elif_ast_nodes);
    }

    #[test]
    #[rustfmt::skip]
    fn test_if_elif_else_expr() {
        let if_elif_expr = "if(block){expression;} else if (block2){expression2;} else {expression3;};";
        let if_elif_ast_nodes = rula::parse(if_elif_expr).unwrap();
        let target_ast_nodes = vec![
            AstNode::RuLa(
                RuLa::new(
                    RuLaKind::Program(
                        Program::new(
                            ProgramKind::Stmt(
                                Stmt::new(
                                    StmtKind::Expr(
                                        Expr::new(
                                            ExprKind::If(
                                                If::new(
                                                    // (block)
                                                    Expr::new(
                                                        ExprKind::Ident(
                                                            Ident::new(
                                                                "block"
                                                            )
                                                        )
                                                    ),
                                                    // {expression}
                                                    Stmt::new(
                                                        StmtKind::Expr(
                                                            Expr::new(
                                                                ExprKind::Ident(
                                                                    Ident::new(
                                                                        "expression"
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    ),
                                                    // elif ~
                                                    Some(
                                                        If::new(
                                                            // else if (block)
                                                            Expr::new(
                                                                ExprKind::Ident(
                                                                    Ident::new(
                                                                        "block2"
                                                                    )
                                                                )
                                                            ),
                                                            // else if () {statement2;};
                                                            Stmt::new(
                                                                StmtKind::Expr(
                                                                    Expr::new(
                                                                        ExprKind::Ident(
                                                                            Ident::new(
                                                                                "expression2"
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            ),
                                                            None,
                                                            None,
                                                        )
                                                    ),
                                                    // else ~
                                                    Some(
                                                        Stmt::new(
                                                            StmtKind::Expr(
                                                                Expr::new(
                                                                    ExprKind::Ident(
                                                                        Ident::new(
                                                                            "expression3"
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        ];
        assert_eq!(target_ast_nodes, if_elif_ast_nodes);
    }
}
