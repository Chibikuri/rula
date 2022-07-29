extern crate rula;
use rula::parser::ast::AstNode;

mod import_tests {
    use super::*;
    #[test]
    fn test_single_import() {
        let import_expr = "import hello";
        let ast_nodes = rula::parse(import_expr).unwrap();
        let expected_paths = vec![["hello"].iter().collect()];
        let expected_ast_nodes = vec![AstNode::Import {
            paths: expected_paths,
        }];
        assert_eq!(ast_nodes.len(), 1);
        assert_eq!(ast_nodes[0], expected_ast_nodes[0]);
    }

    #[test]
    fn test_nested_import() {
        let import_expr = "import hello::world";
        let ast_nodes = rula::parse(import_expr).unwrap();
        let expected_paths = vec![["hello", "world"].iter().collect()];
        let expected_ast_nodes = vec![AstNode::Import {
            paths: expected_paths,
        }];
        assert_eq!(ast_nodes[0], expected_ast_nodes[0]);
    }

    #[test]
    fn test_multiple_import() {
        let import_expr = "import hello::{world, there}";
        let ast_nodes = rula::parse(import_expr).unwrap();

        let expected_path_hello_world = ["hello", "world"].iter().collect();
        let expected_path_hello_there = ["hello", "there"].iter().collect();

        let expected_paths = vec![expected_path_hello_world, expected_path_hello_there];
        let expected_ast_nodes = vec![AstNode::Import {
            paths: expected_paths,
        }];
        assert_eq!(ast_nodes[0], expected_ast_nodes[0]);
    }

    #[test]
    fn test_import_syntax_error_imcomplete_path() {
        // Error test
        // imcompelete path
        let import_expr = "import hello::";
        let error_ast_nodes = rula::parse(import_expr);
        assert!(error_ast_nodes.is_err());
    }

    #[test]
    fn test_import_syntax_error_imcomplete_path_multiple(){
        // imcomplete error
        let import_expr = "import hello::{world, }";
        let error_ast_nodes = rula::parse(import_expr);
        assert!(error_ast_nodes.is_err());
    }
}

mod comment_tests {
    use super::*;
    #[test]
    fn test_implicit_parse_comment() {
        let comment = "// this is a comment \n";
        let empty_ast_nodes = rula::parse(comment).unwrap();
        let target_ast_nodes = vec![AstNode::Eoi];
        assert_eq!(empty_ast_nodes.len(), 1);
        assert_eq!(empty_ast_nodes[0], target_ast_nodes[0]);
    }

    #[test]
    fn test_parse_long_comment() {
        let comment = "/* this is a comment */";
        let empty_ast_nodes = rula::parse(comment).unwrap();
        let target_ast_nodes = vec![AstNode::Eoi];
        assert_eq!(empty_ast_nodes.len(), 1);
        assert_eq!(empty_ast_nodes[0], target_ast_nodes[0]);
    }

    #[test]
    fn test_parse_long_comment_with_new_line() {
        let comment = "/* this is a comment\n hello world\n */";
        let empty_ast_nodes = rula::parse(comment).unwrap();
        let target_ast_nodes = vec![AstNode::Eoi];
        assert_eq!(empty_ast_nodes.len(), 1);
        assert_eq!(empty_ast_nodes[0], target_ast_nodes[0]);
    }
}
