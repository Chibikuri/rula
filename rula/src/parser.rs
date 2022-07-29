pub mod ast;
pub mod error;
pub mod token;

use crate::Rule;
use ast::AstNode;
use error::{IResult, RuLaSyntaxError};
use std::path::PathBuf;

pub fn build_ast_from_program(pair: pest::iterators::Pair<Rule>) -> IResult<AstNode> {
    match pair.as_rule() {
        Rule::stmt => build_ast_from_stmt(pair.into_inner().next().unwrap()),
        _ => Err(RuLaSyntaxError),
    }
}

fn build_ast_from_stmt(pair: pest::iterators::Pair<Rule>) -> IResult<AstNode> {
    match pair.as_rule() {
        Rule::expr => build_ast_from_expr(pair.into_inner().next().unwrap()),
        _ => Err(RuLaSyntaxError),
    }
}

fn build_ast_from_expr(pair: pest::iterators::Pair<Rule>) -> IResult<AstNode> {
    match pair.as_rule() {
        Rule::import_expr => build_ast_from_import_expr(pair),
        Rule::if_expr => build_ast_from_if_expr(pair.into_inner().next().unwrap()),
        _ => Err(RuLaSyntaxError),
    }
}

fn build_ast_from_import_expr(pair: pest::iterators::Pair<Rule>) -> IResult<AstNode> {
    // println!("inside {:#?}", pair);
    // path format validation
    let path: PathBuf = pair.into_inner().map(|x| x.as_str()).into_iter().collect();
    Ok(AstNode::Import { path })
}

fn build_ast_from_if_expr(pair: pest::iterators::Pair<Rule>) -> IResult<AstNode> {
    Ok(AstNode::Test)
}

fn build_ast_from_term(pair: pest::iterators::Pair<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::expr => build_ast_from_term(pair.into_inner().next().unwrap()),
        Rule::number => {
            let mut pair = pair.into_inner();
            let verb = pair.next().unwrap();
            let expr = pair.next().unwrap();
            let expr = build_ast_from_term(expr);
            parse_monadic_verb(verb, expr)
        }
        _ => todo!(), // ... other cases elided here ...
    }
}

fn parse_monadic_verb(pair: pest::iterators::Pair<Rule>, expr: AstNode) -> AstNode {
    AstNode::Test
}
// fn parse_monadic_verb(pair: pest::iterators::Pair<Rule>, expr: AstNode) -> AstNode {
//     AstNode::MonadicOp {
//         verb: match pair.as_str() {
//             ">:" => MonadicVerb::Increment,
//             "*:" => MonadicVerb::Square,
//             "-" => MonadicVerb::Negate,
//             "%" => MonadicVerb::Reciprocal,
//             "#" => MonadicVerb::Tally,
//             ">." => MonadicVerb::Ceiling,
//             "$" => MonadicVerb::ShapeOf,
//             _ => panic!("Unsupported monadic verb: {}", pair.as_str()),
//         },
//         expr: Box::new(expr),
//     }
// }
