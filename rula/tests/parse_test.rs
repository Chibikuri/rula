extern crate rula;

use rula::parser::ast::{AstNode, OpCode};

#[test]
fn test_parse_simple_plus_op() {
    let expr = "1 + 2";
    let ast_nodes = rula::parse(expr).unwrap();
    let mut expected_ast_nodes = vec![
        AstNode::Term {
            lhs: Box::new(AstNode::Integer(1)),
            op: OpCode::Add,
            rhs: Box::new(AstNode::Integer(2)),
        }
    ];
    assert_eq!(ast_nodes.len(), 3);
    // assert_eq!(ast_nodes[0], expected_ast_nodes[0]);
    // let expr = "1+2";
}
