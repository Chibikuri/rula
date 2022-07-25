mod parser;

#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub rula, "/parser/rula.rs");

fn main() {
    println!("Hello, world!");
}

#[test]
fn test_rula() {
    let expr = rula::ExprParser::new().parse("22 * 44 + 66").unwrap();
    assert_eq!(&format!("{:?}", expr), "((22 * 44) + 66)");
}