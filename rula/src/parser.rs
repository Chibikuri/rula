pub mod ast;
pub mod error;
pub mod token;

use crate::Rule;
use ast::AstNode;


pub fn build_ast_from_rula(pair: pest::iterators::Pair<Rule>) -> AstNode {
    match pair.as_rule(){
        Rule::stmt => build_ast_from_stmt(pair.into_inner().next().unwrap()),
        _ => todo!("Should not be here for now")
    }
}


fn build_ast_from_stmt(pair: pest::iterators::Pair<Rule>) -> AstNode{
    match pair.as_rule() {
        Rule::expr => build_ast_from_expr(pair.into_inner().next().unwrap()),
        _ => todo!()
    }
}


fn build_ast_from_expr(pair: pest::iterators::Pair<Rule>) -> AstNode {
    match pair.as_rule(){
        Rule::import_expr => build_ast_from_import_expr(pair.into_inner().next().unwrap()),
        Rule::if_expr => build_ast_from_if_expr(pair.into_inner().next().unwrap()),
        _=> todo!()
    }
}

fn build_ast_from_import_expr(pair: pest::iterators::Pair<Rule>) -> AstNode {
    AstNode::Test
}

fn build_ast_from_if_expr(pair: pest::iterators::Pair<Rule>) -> AstNode {
    AstNode::Test
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
        _ => todo!()
        // ... other cases elided here ...
    }
}

fn parse_monadic_verb(pair: pest::iterators::Pair<Rule>, expr: AstNode) -> AstNode{
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
