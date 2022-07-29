extern crate rula;

use std::path::PathBuf;

use pest::error::Error;
use rula::parser::ast::{AstNode, OpCode};
use rula::parser::error::RuLaSyntaxError;

#[test]
fn test_parse_import() {
    let import_expr = "import hello";
    let ast_nodes = rula::parse(import_expr).unwrap();
    let expect_path = ["hello"].iter().collect();
    let expected_ast_nodes = vec![AstNode::Import { path: expect_path }];
    assert_eq!(ast_nodes.len(), 1);
    assert_eq!(ast_nodes[0], expected_ast_nodes[0]);

    let import_expr = "import hello::world";
    let ast_nodes = rula::parse(import_expr).unwrap();
    let expect_path = ["hello", "world"].iter().collect();
    let expected_ast_nodes = vec![AstNode::Import { path: expect_path }];
    assert_eq!(ast_nodes[0], expected_ast_nodes[0]);

    // Error test
    // imcompelete path
    let import_expr = "import hello::";
    let error_ast_nodes = rula::parse(import_expr);
    assert!(error_ast_nodes.is_err());
}
