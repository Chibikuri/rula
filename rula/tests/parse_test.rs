extern crate rula;
use rula::parser::ast::AstNode;

#[test]
fn test_single_import() {
    let import_expr = "import hello";
    let ast_nodes = rula::parse(import_expr).unwrap();
    let expect_path = ["hello"].iter().collect();
    let expected_ast_nodes = vec![AstNode::Import { path: expect_path }];
    assert_eq!(ast_nodes.len(), 1);
    assert_eq!(ast_nodes[0], expected_ast_nodes[0]);
}

#[test]
fn test_nested_import() {
    let import_expr = "import hello::world";
    let ast_nodes = rula::parse(import_expr).unwrap();
    let expect_path = ["hello", "world"].iter().collect();
    let expected_ast_nodes = vec![AstNode::Import { path: expect_path }];
    assert_eq!(ast_nodes[0], expected_ast_nodes[0]);
}
#[test]
#[should_panic]
fn test_import_syntax_error_imcomplete_path() {
    // Error test
    // imcompelete path
    let import_expr = "import hello::";
    let error_ast_nodes = rula::parse(import_expr);
    assert!(error_ast_nodes.is_err());
}

#[test]
fn test_implicit_parse_comment() {
    let comment = "// this is a comment \n";
    let empty_ast_nodes = rula::parse(comment).unwrap();
    let target_ast_nodes = vec![AstNode::Eoi];
    assert_eq!(empty_ast_nodes.len(), 1);
    assert_eq!(empty_ast_nodes[0], target_ast_nodes[0]);
}
