extern crate rula_parser;

use rula_parser::parser::ast::*;
fn build_stmt_ast(statement: Stmt) -> AstNode {
    AstNode::RuLa(RuLa::new(RuLaKind::Program(Program::new(vec![
        ProgramKind::Stmt(statement),
    ]))))
}

mod import_tests {
    use super::*;
    #[test]
    #[rustfmt::skip]
    fn test_single_import() {
        let import_expr = "import hello";
        let ast_nodes = rula_parser::parse(import_expr).unwrap();
        let expected_paths = vec![["hello"].iter().collect()];
        let expected_ast_nodes = 
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
            );
        assert_eq!(ast_nodes, expected_ast_nodes);
    }

    #[test]
    #[rustfmt::skip]
    fn test_nested_import() {
        let import_expr = "import hello::world";
        let ast_nodes = rula_parser::parse(import_expr).unwrap();
        let expected_paths = vec![["hello", "world"].iter().collect()];
        let expected_ast_nodes = 
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
            );
        assert_eq!(ast_nodes, expected_ast_nodes);
    }

    #[test]
    #[rustfmt::skip]
    fn test_multiple_import() {
        let import_expr = "import hello::{world, there}";
        let ast_nodes = rula_parser::parse(import_expr).unwrap();

        let expected_path_hello_world = ["hello", "world"].iter().collect();
        let expected_path_hello_there = ["hello", "there"].iter().collect();

        let expected_paths = vec![expected_path_hello_world, expected_path_hello_there];
        let expected_ast_nodes = 
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
            );
        assert_eq!(ast_nodes, expected_ast_nodes);
    }

    #[test]
    #[rustfmt::skip]
    fn test_import_syntax_error_imcomplete_path() {
        // Error test
        // imcompelete path
        let import_expr = "import hello::;";
        let error_ast_nodes = rula_parser::parse(import_expr);
        assert!(error_ast_nodes.is_err());
    }

    #[test]
    #[rustfmt::skip]
    fn test_import_syntax_error_imcomplete_path_multiple() {
        // imcomplete error
        let import_expr = "import hello::{world, };";
        let error_ast_nodes = rula_parser::parse(import_expr);
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

mod let_stmt_tests {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_simple_let_stmt() {
        let let_stmt = r#"let hello: str = "world""#;
        let let_ast_nodes = rula_parser::parse(let_stmt).unwrap();
        let target_ast_nodes = 
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
            );
        assert_eq!(let_ast_nodes, target_ast_nodes);
    }

    #[test]
    #[rustfmt::skip]
    fn test_simple_let_stmt_int() {
        let let_stmt = r#"let hello:int = 123"#;
        let let_ast_nodes = rula_parser::parse(let_stmt).unwrap();
        let target_ast_nodes = 
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Let(
                        Let::new(
                            Ident::new(
                                "hello",
                                Some(TypeDef::Integer),
                            ),
                            Expr::new(
                                ExprKind::Lit(
                                    Lit::new(
                                        LitKind::NumberLit(
                                            NumberLit::new("123")
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            );
        assert_eq!(let_ast_nodes, target_ast_nodes);
    }

    #[test]
    #[rustfmt::skip]
    fn test_let_with_if_expr(){
        let let_stmt = "let hello = if(block){expression}";
        let let_if_ast_nodes = rula_parser::parse(let_stmt).unwrap();
        let target_ast_nodes = 
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Let(
                        Let::new(
                            Ident::new(
                                "hello",
                                None,
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
                                                            None,
                                                        ) 
                                                    )
                                                )
                                            )
                                        ),
                                        // {expression}
                                        vec![
                                        Stmt::new(
                                            StmtKind::Expr(
                                                Expr::new(
                                                    ExprKind::Lit(
                                                        Lit::new(
                                                            LitKind::Ident(
                                                                Ident::new(
                                                                    "expression",
                                                                    None,
                                                                ) 
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )],
                                        // elif ~
                                        vec![],
                                        // else ~
                                        None,
                                    )
                                )
                            )

                        )
                    )
                )
            );
        assert_eq!(target_ast_nodes, let_if_ast_nodes)
    }
}

mod if_expr_tests {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_single_if_expr() {
        let if_expr = "if(block){expression}";
        let if_ast_nodes = rula_parser::parse(if_expr).unwrap();
        let target_ast_nodes = 
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
                                                        None,
                                                    ) 
                                                )
                                            )
                                        )
                                    ),
                                    // {expression}
                                    vec![Stmt::new(
                                        StmtKind::Expr(
                                            Expr::new(
                                                ExprKind::Lit(
                                                    Lit::new(
                                                        LitKind::Ident(
                                                            Ident::new(
                                                                "expression",
                                                                None,
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )],
                                    // elif ~
                                    vec![],
                                    // else ~
                                    None,
                                )
                            )
                        )
                    )
                )
            );
        assert_eq!(target_ast_nodes, if_ast_nodes);
    }

    #[test]
    #[rustfmt::skip]
    fn test_if_else_expr() {
        let if_else = "if(block){expression}else{expression2}";
        let if_else_ast_nodes = rula_parser::parse(if_else).unwrap();
        let target_ast_nodes = 
            AstNode::RuLa(
                RuLa::new(
                    RuLaKind::Program(
                        Program::new(
                            vec![
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
                                                                        None,
                                                                    ) 
                                                                )
                                                            )
                                                        )
                                                    ),
                                                    // {expression}
                                                    vec![Stmt::new(
                                                        StmtKind::Expr(
                                                            Expr::new(
                                                                ExprKind::Lit(
                                                                    Lit::new(
                                                                        LitKind::Ident(
                                                                            Ident::new(
                                                                                "expression",
                                                                                None,
                                                                            ) 
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )],
                                                    // elif ~
                                                    vec![],
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
                                    )
                                )
                            )
                            ]
                        )
                    )
                )
            );
        assert_eq!(target_ast_nodes, if_else_ast_nodes);
    }

    #[test]
    #[rustfmt::skip]
    fn test_if_elif_expr() {
        let if_elif_expr = "if(block){expression} else if (block2){expression2}";
        let if_elif_ast_nodes = rula_parser::parse(if_elif_expr).unwrap();
        let target_ast_nodes =
            AstNode::RuLa(
                RuLa::new(
                    RuLaKind::Program(
                        Program::new(
                            vec![
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
                                                                        None,
                                                                    ) 
                                                                )
                                                            )
                                                        )
                                                    ),
                                                    // {expression}
                                                    vec![Stmt::new(
                                                        StmtKind::Expr(
                                                            Expr::new(
                                                                ExprKind::Lit(
                                                                    Lit::new(
                                                                        LitKind::Ident(
                                                                            Ident::new(
                                                                                "expression",
                                                                                None,
                                                                                
                                                                            ) 
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )],
                                                    // elif ~
                                                    vec![Some(
                                                        If::new(
                                                            // else if (block)
                                                            Expr::new(
                                                                ExprKind::Lit(
                                                                    Lit::new(
                                                                        LitKind::Ident(
                                                                            Ident::new(
                                                                                "block2",
                                                                                None,
                                                                            ) 
                                                                        )
                                                                    )
                                                                )
                                                            ),
                                                            // else if () {statement2;};
                                                            vec![Stmt::new(
                                                                StmtKind::Expr(
                                                                    Expr::new(
                                                                        ExprKind::Lit(
                                                                            Lit::new(
                                                                                LitKind::Ident(
                                                                                    Ident::new(
                                                                                        "expression2",
                                                                                        None,
                                                                                        
                                                                                    ) 
                                                                                )
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )],
                                                            vec![],
                                                            None,
                                                        ),
                                                    )],
                                                    // else ~
                                                    None,
                                                )
                                            )
                
                                        )
                                    )
                                )
                            )
                            ]
                        )
                    )
                )
            );
        assert_eq!(target_ast_nodes, if_elif_ast_nodes);
    }

    #[test]
    #[rustfmt::skip]
    fn test_if_elif_else_expr() {
        let if_elif_expr = "if(block){expression} else if (block2){expression2} else {expression3}";
        let if_elif_ast_nodes = rula_parser::parse(if_elif_expr).unwrap();
        let target_ast_nodes = 
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
                                                        None,
                                                        
                                                    ) 
                                                )
                                            )
                                        )
                                    ),
                                    // {expression}
                                    vec![Stmt::new(
                                        StmtKind::Expr(
                                            Expr::new(
                                                ExprKind::Lit(
                                                    Lit::new(
                                                        LitKind::Ident(
                                                            Ident::new(
                                                                "expression",
                                                                None,
                                                            ) 
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )],
                                    // elif ~
                                    vec![Some(
                                        If::new(
                                            // else if (block)
                                            Expr::new(
                                                ExprKind::Lit(
                                                    Lit::new(
                                                        LitKind::Ident(
                                                            Ident::new(
                                                                "block2",
                                                                None,
                                                            ) 
                                                        )
                                                    )
                                                )
                                            ),
                                            // else if () {statement2;};
                                            vec![Stmt::new(
                                                StmtKind::Expr(
                                                    Expr::new(
                                                        ExprKind::Lit(
                                                            Lit::new(
                                                                LitKind::Ident(
                                                                    Ident::new(
                                                                        "expression2",
                                                                        None,
                                                                    ) 
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )],
                                            vec![],
                                            None,
                                        )
                                    )],
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
                    )
                )
            );
        assert_eq!(target_ast_nodes, if_elif_ast_nodes);
    }
    // Add error test here
}

mod test_ruleset_expr {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_simple_ruleset(){
        let ruleset_expr = "ruleset entanglement_swapping{
            stmts
        }";
        let ruleset_expr_asts = rula_parser::parse(ruleset_expr).unwrap();

        let target_ast_nodes = 
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::RuleSetExpr(
                                RuleSetExpr::new(
                                    Ident::new(
                                        "entanglement_swapping",
                                        None,
                                    ),
                                    vec![
                                    Stmt::new(
                                        StmtKind::Expr(
                                            Expr::new(
                                                ExprKind::Lit(
                                                    Lit::new(
                                                        LitKind::Ident(
                                                            Ident::new(
                                                                "stmts",
                                                                None
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )]
                                )
                            )
                        )
                    )
                )
            );
        assert_eq!(ruleset_expr_asts, target_ast_nodes);
    }
}

mod test_rule_expr {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_simple_rule_expr() {
        let rule_expr = "rule hello<#rep>(q2: qubit, q3: qubit) :-> qubit? {cond{} => act{}}";
        let rule_expr_asts = rula_parser::parse(rule_expr).unwrap();
        let target_ast_nodes = 
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::RuleExpr(
                                RuleExpr::new(
                                    Ident::new(
                                        "hello",
                                        None,
                                    ),
                                    Ident::new(
                                        "rep",
                                        None,
                                        
                                    ),
                                vec![
                                    Ident::new(
                                        "q2",
                                        Some(TypeDef::Qubit),
                                        
                                    ),
                                    Ident::new(
                                        "q3",
                                        Some(TypeDef::Qubit),
                                        
                                    )
                                ],
                                Some(ReturnTypeAnnotation::new(
                                    vec![(TypeDef::Qubit, true)],
                                )),
                                RuleContentExpr::new(
                                    vec![],
                                    CondExpr::new(
                                        None,
                                        vec![],
                                    ),
                                    ActExpr::new(
                                        None,
                                        vec![]
                                    ),
                                    vec![]
                                )
                            )
                        )
                    )
                )
            )
        );
        assert_eq!(target_ast_nodes, rule_expr_asts);
    }
}

mod for_expr_test {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_simple_for_expr() {
        // divition is tricky a little
        let term_expr = "for i in range(){hello}";
        let fn_def_asts = rula_parser::parse(term_expr).unwrap();
        let target_ast_nodes =
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::For(
                                For::new(
                                    vec![
                                        Ident::new(
                                            "i",
                                            None,
                                            
                                        )
                                    ],
                                    ForGenerator::Expr(
                                        Expr::new(
                                            ExprKind::FnCall(
                                                FnCall::new(
                                                    Ident::new(
                                                        "range",
                                                        None,
                                                    ),
                                                    false,
                                                    vec![]
                                                )
                                            )
                                        )
                                    ),
                                    vec![Stmt::new(
                                        StmtKind::Expr(
                                            Expr::new(
                                                ExprKind::Lit(
                                                    Lit::new(
                                                        LitKind::Ident(
                                                            Ident::new(
                                                                "hello",
                                                                None,
                                                                
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )]
                                )
                            )
                        )
                    )
                )
            );
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    #[rustfmt::skip]
    fn test_multi_arg_for_expr() {
        // divition is tricky a little
        let term_expr = "for (a, b, c) in generator{hello}";
        let fn_def_asts = rula_parser::parse(term_expr).unwrap();
        let target_ast_nodes = 
            build_stmt_ast(
                Stmt::new(
                    StmtKind::Expr(
                        Expr::new(
                            ExprKind::For(
                                For::new(
                                    vec![
                                        Ident::new(
                                            "a",
                                            None,
                                            
                                        ),
                                        Ident::new(
                                            "b",
                                            None,
                                            
                                        ),
                                        Ident::new(
                                            "c",
                                            None,
                                            
                                        )
                                    ],
                                    ForGenerator::Expr(
                                        Expr::new(
                                            ExprKind::Lit(
                                                Lit::new(
                                                    LitKind::Ident(
                                                        Ident::new(
                                                            "generator",
                                                            None,
                                                            
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    ),
                                    vec![Stmt::new(
                                        StmtKind::Expr(
                                            Expr::new(
                                                ExprKind::Lit(
                                                    Lit::new(
                                                        LitKind::Ident(
                                                            Ident::new(
                                                                "hello",
                                                                None,
                                                                
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )]
                                )
                            )
                        )
                    )
                )
            );
        assert_eq!(target_ast_nodes, fn_def_asts);
    }
}

mod test_match_expr {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_simple_match_no_otherwise(){
        let match_expr = "match test{
            00 => {something()},
            11 => {otherthing()},
        }";
        let match_asts = rula_parser::parse(match_expr).unwrap();
        let target_ast_nodes = build_stmt_ast(
            Stmt::new(
                StmtKind::Expr(
                    Expr::new(
                        ExprKind::Match(
                            Match::new(
                                Expr::new(
                                    ExprKind::Lit(
                                        Lit::new(
                                            LitKind::Ident(
                                                Ident::new(
                                                    "test",
                                                    None
                                                )
                                            )
                                        )
                                    )
                                ),
                                vec![
                                    MatchArm::new(
                                        MatchCondition::new(
                                            Satisfiable::Lit(
                                                Lit::new(
                                                    LitKind::NumberLit(
                                                        NumberLit::new("00")
                                                    )
                                                )
                                            )
                                        ),
                                        MatchAction::new(
                                            vec![
                                                Expr::new(
                                                    ExprKind::FnCall(
                                                        FnCall::new(
                                                            Ident::new("something", None),
                                                            false,
                                                            vec![], 
                                                        )
                                                    )
                                                )
                                            ]
                                        )
                                    ),
                                    MatchArm::new(
                                        MatchCondition::new(
                                            Satisfiable::Lit(
                                                Lit::new(
                                                    LitKind::NumberLit(
                                                        NumberLit::new("11")
                                                    )
                                                )
                                            )
                                        ),
                                        MatchAction::new(
                                            vec![
                                                Expr::new(
                                                    ExprKind::FnCall(
                                                        FnCall::new(
                                                            Ident::new("otherthing", None),
                                                            false,
                                                            vec![], 
                                                        )
                                                    )
                                                )   
                                            ]
                                        )
                                    )
                                ],
                                None
                            )
                        )
                    )
                )
            )
        );
        assert_eq!(target_ast_nodes, match_asts);
    }

    #[test]
    #[rustfmt::skip]
    fn test_simple_match_with_otherwise(){
        let match_expr = "match test{
            00 => {something()},
            11 => {otherthing()},
            otherwise => {final()}
        }";
        let match_asts = rula_parser::parse(match_expr).unwrap();
        let target_ast_nodes = build_stmt_ast(
            Stmt::new(
                StmtKind::Expr(
                    Expr::new(
                        ExprKind::Match(
                            Match::new(
                                Expr::new(
                                    ExprKind::Lit(
                                        Lit::new(
                                            LitKind::Ident(
                                                Ident::new(
                                                    "test",
                                                    None
                                                )
                                            )
                                        )
                                    )
                                ),
                                vec![
                                    MatchArm::new(
                                        MatchCondition::new(
                                            Satisfiable::Lit(
                                                Lit::new(
                                                    LitKind::NumberLit(
                                                        NumberLit::new("00")
                                                    )
                                                )
                                            )
                                        ),
                                        MatchAction::new(
                                            vec![
                                                Expr::new(
                                                    ExprKind::FnCall(
                                                        FnCall::new(
                                                            Ident::new("something", None),
                                                            false,
                                                            vec![], 
                                                        )
                                                    )
                                                )   
                                            ]
                                        )
                                    ),
                                    MatchArm::new(
                                        MatchCondition::new(
                                            Satisfiable::Lit(
                                                Lit::new(
                                                    LitKind::NumberLit(
                                                        NumberLit::new("11")
                                                    )
                                                )
                                            )
                                        ),
                                        MatchAction::new(
                                            vec![
                                                Expr::new(
                                                    ExprKind::FnCall(
                                                        FnCall::new(
                                                            Ident::new("otherthing", None),
                                                            false,
                                                            vec![], 
                                                        )
                                                    )
                                                )   
                                            ]
                                        )
                                    )
                                ],
                                Some(
                                    MatchAction::new(
                                        vec![
                                            Expr::new(
                                                ExprKind::FnCall(
                                                    FnCall::new(
                                                        Ident::new(
                                                            "final", None), 
                                                            false,
                                                             vec![]
                                                            )
                                                        )
                                                    )
                                                    ]
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    );
        assert_eq!(target_ast_nodes, match_asts);
    }
}

mod test_return_expr {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_simple_return_expr() {

        let struct_expr = "return hello";
        let fn_def_asts = rula_parser::parse(struct_expr).unwrap();
        let target_ast_nodes = 
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
            );
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
        let fn_def_asts = rula_parser::parse(lit_expr).unwrap();
        let target_ast_nodes = 
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
            );
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    #[rustfmt::skip]
    fn test_boolean_false_literal() {
        // divition is tricky a little
        let lit_expr = "false";
        let fn_def_asts = rula_parser::parse(lit_expr).unwrap();
        let target_ast_nodes = 
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
            );
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    // helper function
    fn generate_type_lit_ast(name: &str, type_def: Option<TypeDef>) -> AstNode {
        let target_ast_nodes = build_stmt_ast(Stmt::new(StmtKind::Let(Let::new(
            Ident::new(name, type_def),
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "val", None,
            ))))),
        ))));
        target_ast_nodes
    }

    #[test]
    fn test_type_literals() {
        // divition is tricky a little
        let lit_expr = "let number:int = val";
        let fn_def_asts = rula_parser::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_type_lit_ast("number", Some(TypeDef::Integer));
        assert_eq!(target_ast_nodes, fn_def_asts);

        let lit_expr = "let number:float = val";
        let fn_def_asts = rula_parser::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_type_lit_ast("number", Some(TypeDef::Float));
        assert_eq!(target_ast_nodes, fn_def_asts);

        let lit_expr = "let number:u_int = val";
        let fn_def_asts = rula_parser::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_type_lit_ast("number", Some(TypeDef::UnsignedInteger));
        assert_eq!(target_ast_nodes, fn_def_asts);

        // let lit_expr = "let number:complex = val;";
        // let fn_def_asts = rula_parser::parse(lit_expr).unwrap();
        // let target_ast_nodes = generate_type_lit_ast("number", Some(TypeDef::Complex));
        // assert_eq!(target_ast_nodes, fn_def_asts);

        let lit_expr = "let number:bool = val";
        let fn_def_asts = rula_parser::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_type_lit_ast("number", Some(TypeDef::Boolean));
        assert_eq!(target_ast_nodes, fn_def_asts);

        let lit_expr = "let number:str = val";
        let fn_def_asts = rula_parser::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_type_lit_ast("number", Some(TypeDef::Str));
        assert_eq!(target_ast_nodes, fn_def_asts);

        let lit_expr = "let number:qubit = val";
        let fn_def_asts = rula_parser::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_type_lit_ast("number", Some(TypeDef::Qubit));
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    // helper function
    fn generate_lit_ast(literals: Lit) -> AstNode {
        let target_ast_nodes = build_stmt_ast(Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
            literals,
        )))));
        target_ast_nodes
    }

    #[test]
    fn test_binary_literals() {
        let lit_expr = "0b0100100";
        let fn_def_asts = rula_parser::parse(lit_expr).unwrap();
        let target_ast_nodes =
            generate_lit_ast(Lit::new(LitKind::BinaryLit(BinaryLit::new("0100100"))));
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    fn test_hex_literals() {
        let lit_expr = "0x0e8afc";
        let fn_def_asts = rula_parser::parse(lit_expr).unwrap();
        let target_ast_nodes = generate_lit_ast(Lit::new(LitKind::HexLit(HexLit::new("0e8afc"))));
        assert_eq!(target_ast_nodes, fn_def_asts);
    }

    #[test]
    fn test_unicord_literals() {
        let lit_expr = "0u1F680"; //🚀
        let fn_def_asts = rula_parser::parse(lit_expr).unwrap();
        let target_ast_nodes =
            generate_lit_ast(Lit::new(LitKind::UnicordLit(UnicordLit::new("1F680"))));
        assert_eq!(target_ast_nodes, fn_def_asts);
    }
}

#[cfg(test)]
mod test_variable_call {
    use super::*;

    #[test]
    fn test_simple_variable_call() {
        let var_call = "test.hello";
        let var_call_ast = rula_parser::parse(var_call).unwrap();
        let target_ast_nodes = build_stmt_ast(Stmt::new(StmtKind::Expr(Expr::new(
            ExprKind::VariableCallExpr(VariableCallExpr::new(vec![
                Callable::Ident(Ident::new("test", None)),
                Callable::Ident(Ident::new("hello", None)),
            ])),
        ))));
        assert_eq!(target_ast_nodes, var_call_ast);
    }

    #[test]
    fn test_simple_variable_fnc_call() {
        let var_call = "test.hello()";
        let var_call_ast = rula_parser::parse(var_call).unwrap();
        let target_ast_nodes = build_stmt_ast(Stmt::new(StmtKind::Expr(Expr::new(
            ExprKind::VariableCallExpr(VariableCallExpr::new(vec![
                Callable::Ident(Ident::new("test", None)),
                Callable::FnCall(FnCall::new(Ident::new("hello", None), false, vec![])),
            ])),
        ))));
        assert_eq!(target_ast_nodes, var_call_ast);
    }

    // #[test]
    // fn test_simple_variable_fnc_call_first() {
    //     let var_call = "test().hello";
    //     let var_call_ast = rula_parser::parse(var_call).unwrap();
    //     let target_ast_nodes = build_stmt_ast(Stmt::new(StmtKind::Expr(Expr::new(
    //         ExprKind::VariableCallExpr(VariableCallExpr::new(vec![
    //             Callable::FnCall(FnCall::new(Ident::new("test", None), false, vec![])),
    //             Callable::Ident(Ident::new("hello", None)),
    //         ])),
    //     ))));
    //     assert_eq!(target_ast_nodes, var_call_ast);
    // }
}
