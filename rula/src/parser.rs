pub mod ast;
pub mod error;
pub mod token;
mod util;

use crate::Rule;
use ast::{AstNode, Expr, ExprKind, Import, PathKind};
use ast::{Stmt, StmtKind};
use error::RuLaError;
use std::path::PathBuf;

use pest::iterators::Pair;

// Custome error interface for rula
pub type IResult<T> = std::result::Result<T, RuLaError>;

/**
 * parse rula system and if it matches program, going to `build_ast_from_program`.
*/
pub fn build_ast_from_rula(pair: Pair<Rule>) -> IResult<AstNode> {
    match pair.as_rule() {
        Rule::program => {
            if pair.as_str() == "" {
                return Ok(AstNode::Ignore);
            } else {
                return build_ast_from_program(pair.into_inner().next().unwrap());
            }
        }
        Rule::EOI => Ok(AstNode::Eoi),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

fn build_ast_from_program(pair: Pair<Rule>) -> IResult<AstNode> {
    match pair.as_rule() {
        Rule::stmt => Ok(AstNode::Stmt(
            build_ast_from_stmt(pair.into_inner().next().unwrap()).unwrap(),
        )),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

fn build_ast_from_stmt(pair: Pair<Rule>) -> IResult<Stmt> {
    println!("{:#?}", &pair);
    match pair.as_rule() {
        Rule::let_stmt => Ok(Stmt::new(
            build_ast_from_let_stmt(pair.into_inner().next().unwrap()).unwrap(),
        )),
        Rule::expr => Ok(Stmt::new(
            build_ast_from_expr(pair.into_inner().next().unwrap()).unwrap(),
        )),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

fn build_ast_from_let_stmt(pair: Pair<Rule>) -> IResult<StmtKind> {
    // Ok(AstNode::Test)
    Ok(StmtKind::Let)
}

fn build_ast_from_expr(pair: Pair<Rule>) -> IResult<StmtKind> {
    match pair.as_rule() {
        Rule::if_expr => Ok(StmtKind::Expr(build_ast_from_if_expr(pair).unwrap())),
        Rule::import_expr => Ok(StmtKind::Expr(build_ast_from_import_expr(pair).unwrap())),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

fn build_ast_from_if_expr(pair: pest::iterators::Pair<Rule>) -> IResult<Expr> {
    let mut expr_vec: Vec<Expr> = vec![];
    for expr in pair.into_inner() {
        match expr.as_rule() {
            Rule::paren_expr => {
                // expr_vec.push(build_ast_from_expr(expr.into_inner().next().unwrap()).unwrap());
            }
            Rule::brace_stmt => {}
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(Expr::new(ExprKind::Test))
}

fn build_ast_from_import_expr(pair: Pair<Rule>) -> IResult<Expr> {
    let mut path_list: Vec<PathBuf> = vec![];
    let mut end_paths: Vec<&str> = vec![];
    // ::{a, b} expression must be single not two of them
    // e.g. not allowed ::{a, b}::{c, d}
    // This can be rejected by level of grammar
    let mut path = PathBuf::new();
    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::ident => path.push(inner_pair.as_str()),
            Rule::ident_list => {
                for inner_inner_pair in inner_pair.into_inner() {
                    end_paths.push(inner_inner_pair.as_str());
                }
                // Path must be completed once this is called.
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    // No end component such as `hello::{a, b}`
    // then just add `hello` as a path
    if end_paths.len() == 0 {
        path_list.push(path)
    } else {
        for t_path in end_paths {
            let mut cloned_path = path.clone();
            cloned_path.push(t_path);
            path_list.push(cloned_path);
        }
    }

    Ok(Expr::new(ExprKind::Import(Import::new(PathKind::from(
        path_list,
    )))))
}

// fn build_ast_from_term(pair: pest::iterators::Pair<Rule>) -> AstNode {
//     match pair.as_rule() {
//         Rule::expr => build_ast_from_term(pair.into_inner().next().unwrap()),
//         Rule::number => {
//             let mut pair = pair.into_inner();
//             let verb = pair.next().unwrap();
//             let expr = pair.next().unwrap();
//             let expr = build_ast_from_term(expr);
//             parse_monadic_verb(verb, expr)
//         }
//         _ => todo!(), // ... other cases elided here ...
//     }
// }

// fn parse_monadic_verb(pair: pest::iterators::Pair<Rule>, expr: AstNode) -> AstNode {
//     AstNode::Test
// }
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
