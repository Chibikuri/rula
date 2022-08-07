extern crate rula;

use rula::parser::ast::{
    ActExpr, Array, AstNode, BinaryLit, Comp, CompOpKind, CondExpr, Expr, ExprKind, FnCall, FnDef,
    For, HexLit, Ident, If, Import, Let, Lit, LitKind, NumberLit, PathKind, Program, ProgramKind,
    Return, RuLa, RuLaKind, RuleExpr, Stmt, StmtKind, StringLit, Struct, TypeDef, UnicordLit,
    While,
};

fn build_stmt_ast(statement: Stmt) -> AstNode {
    AstNode::RuLa(RuLa::new(RuLaKind::Program(Program::new(
        ProgramKind::Stmt(statement),
    ))))
}

mod import_tests {
    use super::*;
    #[test]
    #[rustfmt::skip]
    fn test_single_import() {
        let import_expr = "import hello;";
        let ast_nodes = rula::parse(import_expr).unwrap();
        let expected_paths = vec![["hello"].iter().collect()];
        let expected_ast_nodes = vec![
            build_stmt_ast(
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
            build_stmt_ast(
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
            build_stmt_ast(
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
        let let_stmt = r#"let hello: str = "world";"#;
        let let_ast_nodes = rula::parse(let_stmt).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Let(
                        Let::new(
                            Ident::new(
                                "hello",
                                Some(TypeDef::Str),
                            ),
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
            ];
        assert_eq!(let_ast_nodes, target_ast_nodes);
    }

    #[test]
    #[rustfmt::skip]
    fn test_simple_let_stmt_int() {
        let let_stmt = r#"let hello:i32 = 123;"#;
        let let_ast_nodes = rula::parse(let_stmt).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Let(
                        Let::new(
                            Ident::new(
                                "hello",
                                Some(TypeDef::Integer32),
                            ),
                            Expr::new(
                                ExprKind::Term(
                                    123.0
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
        let let_stmt = "let hello = if(block){expression};";
        let let_if_ast_nodes = rula::parse(let_stmt).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Let(
                        Let::new(
                            Ident::new(
                                "hello",
                                None
                            ),
                            Expr::new(
                                ExprKind::If(
                                    If::new(
                                        // (block)
                                        Expr::new(
                                            ExprKind::Lit(
                                                Lit::new(
                                                    LitKind::Ident(
                                                        Ident::new(
                                                            "block",
                                                            None
                                                        ) 
                                                    )
                                                )
                                            )
                                        ),
                                        // {expression}
                                        Stmt::new(
                                            StmtKind::Expr(
                                                Expr::new(
                                                    ExprKind::Lit(
                                                        Lit::new(
                                                            LitKind::Ident(
                                                                Ident::new(
                                                                    "expression",
                                                                    None
                                                                ) 
                                                            )
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
        ];
        assert_eq!(target_ast_nodes, let_if_ast_nodes)
    }
}

mod if_tests {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_single_if_expr() {
        let if_expr = "if(block){expression}";
        let if_ast_nodes = rula::parse(if_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::If(
                                If::new(
                                    // (block)
                                    Expr::new(
                                        ExprKind::Lit(
                                            Lit::new(
                                                LitKind::Ident(
                                                    Ident::new(
                                                        "block",
                                                        None
                                                    ) 
                                                )
                                            )
                                        )
                                    ),
                                    // {expression}
                                    Stmt::new(
                                        StmtKind::Expr(
                                            Expr::new(
                                                ExprKind::Lit(
                                                    Lit::new(
                                                        LitKind::Ident(
                                                            Ident::new(
                                                                "expression",
                                                                None
                                                            )
                                                        )
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
        ];
        assert_eq!(target_ast_nodes, if_ast_nodes);
    }

    #[test]
    #[rustfmt::skip]
    fn test_if_else_expr() {
        let if_else = "if(block){expression}else{expression2}";
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
                                                        ExprKind::Lit(
                                                            Lit::new(
                                                                LitKind::Ident(
                                                                    Ident::new(
                                                                        "block",
                                                                        None
                                                                    ) 
                                                                )
                                                            )
                                                        )
                                                    ),
                                                    // {expression}
                                                    Stmt::new(
                                                        StmtKind::Expr(
                                                            Expr::new(
                                                                ExprKind::Lit(
                                                                    Lit::new(
                                                                        LitKind::Ident(
                                                                            Ident::new(
                                                                                "expression",
                                                                                None
                                                                            ) 
                                                                        )
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
                                                                    ExprKind::Lit(
                                                                        Lit::new(
                                                                            LitKind::Ident(
                                                                                Ident::new(
                                                                                    "expression2",
                                                                                    None
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
                )
            )
        ];
        assert_eq!(target_ast_nodes, if_else_ast_nodes);
    }

    #[test]
    #[rustfmt::skip]
    fn test_if_elif_expr() {
        let if_elif_expr = "if(block){expression} else if (block2){expression2}";
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
                                                        ExprKind::Lit(
                                                            Lit::new(
                                                                LitKind::Ident(
                                                                    Ident::new(
                                                                        "block",
                                                                        None
                                                                    ) 
                                                                )
                                                            )
                                                        )
                                                    ),
                                                    // {expression}
                                                    Stmt::new(
                                                        StmtKind::Expr(
                                                            Expr::new(
                                                                ExprKind::Lit(
                                                                    Lit::new(
                                                                        LitKind::Ident(
                                                                            Ident::new(
                                                                                "expression",
                                                                                None
                                                                            ) 
                                                                        )
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
                                                                ExprKind::Lit(
                                                                    Lit::new(
                                                                        LitKind::Ident(
                                                                            Ident::new(
                                                                                "block2",
                                                                                None
                                                                            ) 
                                                                        )
                                                                    )
                                                                )
                                                            ),
                                                            // else if () {statement2;};
                                                            Stmt::new(
                                                                StmtKind::Expr(
                                                                    Expr::new(
                                                                        ExprKind::Lit(
                                                                            Lit::new(
                                                                                LitKind::Ident(
                                                                                    Ident::new(
                                                                                        "expression2",
                                                                                        None
                                                                                    ) 
                                                                                )
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
        let if_elif_expr = "if(block){expression} else if (block2){expression2} else {expression3}";
        let if_elif_ast_nodes = rula::parse(if_elif_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::If(
                                If::new(
                                    // (block)
                                    Expr::new(
                                        ExprKind::Lit(
                                            Lit::new(
                                                LitKind::Ident(
                                                    Ident::new(
                                                        "block",
                                                        None
                                                    ) 
                                                )
                                            )
                                        )
                                    ),
                                    // {expression}
                                    Stmt::new(
                                        StmtKind::Expr(
                                            Expr::new(
                                                ExprKind::Lit(
                                                    Lit::new(
                                                        LitKind::Ident(
                                                            Ident::new(
                                                                "expression",
                                                                None
                                                            ) 
                                                        )
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
                                                ExprKind::Lit(
                                                    Lit::new(
                                                        LitKind::Ident(
                                                            Ident::new(
                                                                "block2",
                                                                None
                                                            ) 
                                                        )
                                                    )
                                                )
                                            ),
                                            // else if () {statement2;};
                                            Stmt::new(
                                                StmtKind::Expr(
                                                    Expr::new(
                                                        ExprKind::Lit(
                                                            Lit::new(
                                                                LitKind::Ident(
                                                                    Ident::new(
                                                                        "expression2",
                                                                        None
                                                                    ) 
                                                                )
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
                                                    ExprKind::Lit(
                                                        Lit::new(
                                                            LitKind::Ident(
                                                                Ident::new(
                                                                    "expression3",
                                                                    None
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
    // Add error test here
}

mod fn_def_test {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_simple_fn_def() {
        let fn_def_expr = "fn(block:i32){expression}";
        let fn_def_asts = rula::parse(fn_def_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::FnDef(
                                FnDef::new(
                                    vec![
                                        LitKind::Ident(
                                            Ident::new(
                                                "block",
                                                Some(TypeDef::Integer32)
                                            ) 
                                        )
                                        ],
                                        Stmt::new(
                                            StmtKind::Expr(
                                                Expr::new(
                                                    ExprKind::Lit(
                                                        Lit::new(
                                                            LitKind::Ident(
                                                                Ident::new(
                                                                    "expression",
                                                                    None
                                                                ) 
                                                            )
                                                        )
                                                    )
                                                ),
                                            )
                                        )
                                    ),
                                )
                            )
                        )
                    )
                )
                ];
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    #[rustfmt::skip]
    fn test_mlti_args_fn_def() {
        let fn_def_expr = "fn(block:i32, hello:str){expression}";
        let fn_def_asts = rula::parse(fn_def_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::FnDef(
                                FnDef::new(
                                    vec![
                                        LitKind::Ident(
                                            Ident::new(
                                                "block",
                                                Some(TypeDef::Integer32)
                                            ) 
                                        ),
                                        LitKind::Ident(
                                            Ident::new(
                                                "hello",
                                                Some(TypeDef::Str)
                                            ) 
                                        ),
                                        ],
                                        Stmt::new(
                                            StmtKind::Expr(
                                                Expr::new(
                                                    ExprKind::Lit(
                                                        Lit::new(
                                                            LitKind::Ident(
                                                                Ident::new(
                                                                    "expression",
                                                                    None
                                                                ) 
                                                            )
                                                        ),
                                                    )
                                                ),
                                            )
                                        )
                                    ),
                                )
                            )
                        )
                    )
            )
                ];
        assert_eq!(target_ast_nodes, fn_def_asts);
    }
}

mod term_tests {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_simple_term_expr() {
        let term_expr = "1+3";
        let fn_def_asts = rula::parse(term_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::Term(
                                4.0
                            )
                            )
                        )
                    )
                )
                ];
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    #[rustfmt::skip]
    fn test_simple_term_expr_with_order() {
        let term_expr = "4*(3+4)";
        let fn_def_asts = rula::parse(term_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::Term(
                                28.0    
                            )
                        )
                    )
                )
            )
            ];
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    #[rustfmt::skip]
    fn test_simple_term_expr_with_order2() {
        let term_expr = "(1-4)*(3+4)/3";
        let fn_def_asts = rula::parse(term_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::Term(
                                -7.0   
                            )
                        )
                    )
                )
            )
            ];
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    #[rustfmt::skip]
    fn test_edge_case_term_expr() {
        let term_expr = "((12))";
        let fn_def_asts = rula::parse(term_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::Term(
                                12.0
                            )
                        )
                    )
                )
            )
            ];
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    #[rustfmt::skip]
    fn test_complex_term_expr() {
        // divition is tricky a little
        let term_expr = "(1/(3-1))";
        let fn_def_asts = rula::parse(term_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::Term(
                                1.0/(3.0-1.0)
                            )
                        )
                    )
                )
            )
            ];
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    #[rustfmt::skip]
    fn test_complex_term_expr2() {
        // divition is tricky a little
        let term_expr = "(1+(2*(3/(4-(5*(6/(7+8)))))))";
        let fn_def_asts = rula::parse(term_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::Term(
                                1.0+(2.0*(3.0/(4.0-(5.0*(6.0/(7.0+8.0))))))
                            )
                        )
                    )
                )
            )
            ];
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    #[rustfmt::skip]
    fn test_complex_term_expr3() {
        // divition is tricky a little
        let term_expr = "(((((1+2)-3)*4)+5)-6)";
        let fn_def_asts = rula::parse(term_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::Term(
                                (((((1+2)-3)*4)+5)-6) as f64
                            )
                        )
                    )
                )
            )
            ];
        assert_eq!(target_ast_nodes, fn_def_asts);
    }
}

mod for_expr_test {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_simple_for_expr() {
        // divition is tricky a little
        let term_expr = "for (i) in range(){hello}";
        let fn_def_asts = rula::parse(term_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::For(
                                For::new(
                                    vec![
                                        Ident::new(
                                            "i",
                                            None
                                        )
                                    ],
                                    Expr::new(
                                        ExprKind::FnCall(
                                            FnCall::new(
                                                Ident::new(
                                                    "range",
                                                    None
                                                )
                                            )
                                        )
                                    ),
                                    Stmt::new(
                                        StmtKind::Expr(
                                            Expr::new(
                                                ExprKind::Lit(
                                                    Lit::new(
                                                        LitKind::Ident(
                                                            Ident::new(
                                                                "hello",
                                                                None
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
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    #[rustfmt::skip]
    fn test_multi_arg_for_expr() {
        // divition is tricky a little
        let term_expr = "for (a, b, c) in generator{hello}";
        let fn_def_asts = rula::parse(term_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::For(
                                For::new(
                                    vec![
                                        Ident::new(
                                            "a",
                                            None
                                        ),
                                        Ident::new(
                                            "b",
                                            None
                                        ),
                                        Ident::new(
                                            "c",
                                            None
                                        )
                                    ],
                                    Expr::new(
                                        ExprKind::Lit(
                                            Lit::new(
                                                LitKind::Ident(
                                                    Ident::new(
                                                        "generator",
                                                        None
                                                    )
                                                )
                                            )
                                        )
                                    ),
                                    Stmt::new(
                                        StmtKind::Expr(
                                            Expr::new(
                                                ExprKind::Lit(
                                                    Lit::new(
                                                        LitKind::Ident(
                                                            Ident::new(
                                                                "hello",
                                                                None
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
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    #[rustfmt::skip]
    fn test_multi_arg_lists_for_expr() {
        // divition is tricky a little
        let term_expr = "for (i) in [1, 2, 3, 4, 5]{hello}";
        let fn_def_asts = rula::parse(term_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::For(
                                For::new(
                                    vec![
                                        Ident::new(
                                            "i",
                                            None
                                        ),
                                    ],
                                    Expr::new(
                                        ExprKind::Array(
                                            Array::new(
                                                vec![
                                                    Lit::new(
                                                        LitKind::NumberLit(
                                                            NumberLit::new("1")
                                                        )
                                                    ),
                                                    Lit::new(
                                                        LitKind::NumberLit(
                                                            NumberLit::new("2")
                                                        )
                                                    ), 
                                                    Lit::new(
                                                        LitKind::NumberLit(
                                                            NumberLit::new("3")
                                                        )
                                                    ), 
                                                    Lit::new(
                                                        LitKind::NumberLit(
                                                            NumberLit::new("4")
                                                        )
                                                    ), 
                                                    Lit::new(
                                                        LitKind::NumberLit(
                                                            NumberLit::new("5")
                                                        )
                                                    )
                                                    ]   
                                            )
                                        )
                                    ),
                                    Stmt::new(
                                        StmtKind::Expr(
                                            Expr::new(
                                                ExprKind::Lit(
                                                    Lit::new(
                                                        LitKind::Ident(
                                                            Ident::new(
                                                                "hello",
                                                                None
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
        assert_eq!(target_ast_nodes, fn_def_asts);
    }
}

mod test_while_expr {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_while_expr() {

        let while_expr = "while(count > 0){expression}";
        let fn_def_asts = rula::parse(while_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::While(
                                While::new(
                                    Expr::new(
                                        ExprKind::Comp(
                                            Comp::new(
                                                Expr::new(
                                                    ExprKind::Lit(
                                                        Lit::new(
                                                        LitKind::Ident(
                                                            Ident::new(
                                                                "count",
                                                                None
                                                            )
                                                        )
                                                    )
                                                )
                                            ),
                                            CompOpKind::Gt,
                                            Expr::new(
                                                ExprKind::Term(
                                                    0.0
                                                )
                                            )
                                        )
                                    )
                                ),
                                Stmt::new(
                                    StmtKind::Expr(
                                        Expr::new(
                                            ExprKind::Lit(
                                                Lit::new(
                                                    LitKind::Ident(
                                                        Ident::new(
                                                            "expression",
                                                            None
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
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    #[rustfmt::skip]
    fn test_while_expr_ident() {

        let while_expr = "while(count){expression}";
        let fn_def_asts = rula::parse(while_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::While(
                                While::new(
                                    Expr::new(
                                        ExprKind::Lit(
                                            Lit::new(
                                                LitKind::Ident(
                                                    Ident::new(
                                                        "count",
                                                        None,
                                                    )
                                                )
                                            )
                                        )
                                    ),
                                    Stmt::new(
                                        StmtKind::Expr(
                                            Expr::new(
                                                ExprKind::Lit(
                                                    Lit::new(
                                                        LitKind::Ident(
                                                            Ident::new(
                                                                "expression",
                                                                None
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
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    #[rustfmt::skip]
    fn test_while_expr_literal() {

        let while_expr = "while(true){expression}";
        let fn_def_asts = rula::parse(while_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::While(
                                While::new(
                                    Expr::new(
                                        ExprKind::Lit(
                                            Lit::new(
                                                LitKind::BooleanLit(
                                                    true
                                                )
                                            )
                                        )
                                    ),
                                    Stmt::new(
                                        StmtKind::Expr(
                                            Expr::new(
                                                ExprKind::Lit(
                                                    Lit::new(
                                                        LitKind::Ident(
                                                            Ident::new(
                                                                "expression",
                                                                None
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
        assert_eq!(target_ast_nodes, fn_def_asts);
    }
}

mod test_struct_expr {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_empty_struct_expr() {

        let struct_expr = "struct Test{}";
        let fn_def_asts = rula::parse(struct_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::Struct(
                                Struct::new(
                                    Ident::new(
                                        "Test",
                                        None
                                    ),
                                    vec![]
                                )
                            )
                        )
                    )
                )
            )
            ];
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    #[rustfmt::skip]
    fn test_simple_struct_expr() {

        let struct_expr = "struct Test{flag: bool}";
        let fn_def_asts = rula::parse(struct_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::Struct(
                                Struct::new(
                                    Ident::new(
                                        "Test",
                                        None
                                    ),
                                    vec![
                                        Ident::new(
                                            "flag",
                                            Some(TypeDef::Boolean)
                                        )
                                    ]
                                )
                            )
                        )
                    )
                )
            )
            ];
        assert_eq!(target_ast_nodes, fn_def_asts);
    }
}

mod test_return_expr {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_simple_return_expr() {

        let struct_expr = "return hello";
        let fn_def_asts = rula::parse(struct_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::Return(
                                Return::new(
                                    Expr::new(
                                        ExprKind::Lit(
                                            Lit::new(
                                                LitKind::Ident(
                                                    Ident::new(
                                                        "hello",
                                                        None
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
        assert_eq!(target_ast_nodes, fn_def_asts);
    }
}

mod test_rule_expr {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_simple_rule_expr() {

        let rule_expr = "rule<q: qubit>(q2: qubit, q3: qubit){expression}";
        let fn_def_asts = rula::parse(rule_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::RuleExpr(
                                RuleExpr::new(
                                    Ident::new(
                                        "q",
                                        Some(TypeDef::Qubit),
                                    ),
                                vec![
                                    Ident::new(
                                        "q2",
                                        Some(TypeDef::Qubit)
                                    ),
                                    Ident::new(
                                        "q3",
                                        Some(TypeDef::Qubit),
                                    )
                                ],
                                Stmt::new(
                                    StmtKind::Expr(
                                        Expr::new(
                                            ExprKind::Lit(
                                                Lit::new(
                                                    LitKind::Ident(
                                                        Ident::new(
                                                            "expression",
                                                            None
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
        assert_eq!(target_ast_nodes, fn_def_asts);
    }
}

mod test_condition_expr {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_simple_cond_expr() {
        let cond_expr = "cond condition1 {awaitables}";
        let fn_def_asts = rula::parse(cond_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::CondExpr(
                                CondExpr::new(
                                    Some(
                                        Ident::new(
                                            "condition1",
                                            None,
                                        )
                                    ),
                                    Stmt::new(
                                        StmtKind::Expr(
                                            Expr::new(
                                                ExprKind::Lit(
                                                    Lit::new(
                                                        LitKind::Ident(
                                                            Ident::new(
                                                                "awaitables",
                                                                None
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
        assert_eq!(target_ast_nodes, fn_def_asts); 
    }
}

mod test_action_expr {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_simple_act_expr() {
        let act_expr = "act act1 {operatable}";
        let fn_def_asts = rula::parse(act_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::ActExpr(
                                ActExpr::new(
                                    Some(
                                        Ident::new(
                                            "act1",
                                            None,
                                        )
                                    ),
                                    Stmt::new(
                                        StmtKind::Expr(
                                            Expr::new(
                                                ExprKind::Lit(
                                                    Lit::new(
                                                        LitKind::Ident(
                                                            Ident::new(
                                                                "operatable",
                                                                None
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
        assert_eq!(target_ast_nodes, fn_def_asts); 
    }
}

mod test_literals {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_boolean_true_literal() {
        // divition is tricky a little
        let lit_expr = "true";
        let fn_def_asts = rula::parse(lit_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::Lit(
                                Lit::new(
                                    LitKind::BooleanLit(true)
                                )
                            )
                        )
                    )
                )
            )
            ];
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    #[rustfmt::skip]
    fn test_boolean_false_literal() {
        // divition is tricky a little
        let lit_expr = "false";
        let fn_def_asts = rula::parse(lit_expr).unwrap();
        let target_ast_nodes = vec![
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::Lit(
                                Lit::new(
                                    LitKind::BooleanLit(false)
                                )
                            )
                        )
                    )
                )
            )
            ];
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    // helper function
    fn generate_type_lit_ast(name: &str, type_def: Option<TypeDef>) -> Vec<AstNode> {
        let target_ast_nodes = vec![build_stmt_ast(Stmt::new(StmtKind::Let(Let::new(
            Ident::new(name, type_def),
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "val", None,
            ))))),
        ))))];
        target_ast_nodes
    }

    #[test]
    fn test_type_literals() {
        // divition is tricky a little
        let lit_expr = "let integer:i32 = val;";
        let fn_def_asts = rula::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_type_lit_ast("integer", Some(TypeDef::Integer32));
        assert_eq!(target_ast_nodes, fn_def_asts);

        let lit_expr = "let integer:i64 = val;";
        let fn_def_asts = rula::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_type_lit_ast("integer", Some(TypeDef::Integer64));
        assert_eq!(target_ast_nodes, fn_def_asts);

        let lit_expr = "let integer:f32 = val;";
        let fn_def_asts = rula::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_type_lit_ast("integer", Some(TypeDef::Float32));
        assert_eq!(target_ast_nodes, fn_def_asts);

        let lit_expr = "let integer:f64 = val;";
        let fn_def_asts = rula::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_type_lit_ast("integer", Some(TypeDef::Float64));
        assert_eq!(target_ast_nodes, fn_def_asts);

        let lit_expr = "let integer:u32 = val;";
        let fn_def_asts = rula::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_type_lit_ast("integer", Some(TypeDef::UnsignedInteger32));
        assert_eq!(target_ast_nodes, fn_def_asts);

        let lit_expr = "let integer:u64 = val;";
        let fn_def_asts = rula::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_type_lit_ast("integer", Some(TypeDef::UnsignedInteger64));
        assert_eq!(target_ast_nodes, fn_def_asts);

        let lit_expr = "let integer:c128 = val;";
        let fn_def_asts = rula::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_type_lit_ast("integer", Some(TypeDef::Complex128));
        assert_eq!(target_ast_nodes, fn_def_asts);

        let lit_expr = "let integer:bool = val;";
        let fn_def_asts = rula::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_type_lit_ast("integer", Some(TypeDef::Boolean));
        assert_eq!(target_ast_nodes, fn_def_asts);

        let lit_expr = "let integer:str = val;";
        let fn_def_asts = rula::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_type_lit_ast("integer", Some(TypeDef::Str));
        assert_eq!(target_ast_nodes, fn_def_asts);

        let lit_expr = "let integer:qubit = val;";
        let fn_def_asts = rula::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_type_lit_ast("integer", Some(TypeDef::Qubit));
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    // helper function
    fn generate_lit_ast(literals: Lit) -> Vec<AstNode> {
        let target_ast_nodes = vec![build_stmt_ast(Stmt::new(StmtKind::Expr(Expr::new(
            ExprKind::Lit(literals),
        ))))];
        target_ast_nodes
    }

    #[test]
    fn test_binary_literals() {
        let lit_expr = "0b0100100";
        let fn_def_asts = rula::parse(lit_expr).unwrap();
        let target_ast_nodes =
            generate_lit_ast(Lit::new(LitKind::BinaryLit(BinaryLit::new("0100100"))));
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    fn test_hex_literals() {
        let lit_expr = "0x0e8afc";
        let fn_def_asts = rula::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_lit_ast(Lit::new(LitKind::HexLit(HexLit::new("0e8afc"))));
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    fn test_unicord_literals() {
        let lit_expr = "0u1F680"; //🚀
        let fn_def_asts = rula::parse(lit_expr).unwrap();
        let target_ast_nodes =
            generate_lit_ast(Lit::new(LitKind::UnicordLit(UnicordLit::new("1F680"))));
        assert_eq!(target_ast_nodes, fn_def_asts);
    }
}
