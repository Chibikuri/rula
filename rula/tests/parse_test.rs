extern crate rula;

use rula::parser::ast::AstNode;

#[test]
fn test_parse() {
    let expr = "if (5 > 1){ let i = 10 }";
    let ast = rula::parse(expr).unwrap();
    let mut expect = vec![];
    let target_ast = AstNode::Test;
    expect.push(target_ast);
    assert_eq!(ast.len(), 1);
    assert_eq!(ast[0], expect[0]);
}
