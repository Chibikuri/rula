pub mod ast;
pub mod error;
pub mod token;
mod util;

use crate::Rule;
// Statements
use ast::{Let, Stmt, StmtKind};
// Expressions
use ast::{AstNode, Expr, ExprKind, Ident, If, Import, PathKind, StringLit};
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
    match pair.as_rule() {
        Rule::let_stmt => Ok(Stmt::new(build_ast_from_let_stmt(pair).unwrap())),
        Rule::expr => Ok(Stmt::new(
            build_ast_from_expr(pair.into_inner().next().unwrap()).unwrap(),
        )),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

fn build_ast_from_let_stmt(pair: Pair<Rule>) -> IResult<StmtKind> {
    // Ugh ugly
    let mut identity = ExprKind::Ident(Ident::place_holder());
    let mut expr = StmtKind::Expr(Expr::place_holder());
    for let_pair in pair.into_inner() {
        match let_pair.as_rule() {
            Rule::ident => {
                identity = build_ast_from_ident(let_pair).unwrap();
            }
            Rule::expr => {
                expr = build_ast_from_expr(let_pair.into_inner().next().unwrap()).unwrap();
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
            // _ => todo!(),
        }
    }
    // Should have easier way
    if identity == ExprKind::Ident(Ident::place_holder())
        || expr == StmtKind::Expr(Expr::place_holder())
    {
        return Err(RuLaError::RuLaSyntaxError);
    }
    Ok(StmtKind::Let(Let::new(identity, expr)))
}

fn build_ast_from_ident(pair: Pair<Rule>) -> IResult<ExprKind> {
    return Ok(ExprKind::Ident(Ident::new(pair.as_str())));
}

fn build_ast_from_expr(pair: Pair<Rule>) -> IResult<StmtKind> {
    match pair.as_rule() {
        Rule::if_expr => Ok(StmtKind::Expr(Expr::new(
            build_ast_from_if_expr(pair).unwrap(),
        ))),
        // Should be recursive?
        // Rule::paren_expr => Ok(StmtKind::Expr(build_ast_from_paren_expr(pair).unwrap())),
        Rule::import_expr => Ok(StmtKind::Expr(build_ast_from_import_expr(pair).unwrap())),
        Rule::literals => Ok(StmtKind::Expr(Expr::new(
            build_ast_from_literals(pair.into_inner().next().unwrap()).unwrap(),
        ))),
        Rule::ident => Ok(StmtKind::Expr(Expr::new(
            build_ast_from_ident(pair).unwrap(),
        ))),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

fn build_ast_from_literals(pair: Pair<Rule>) -> IResult<ExprKind> {
    match pair.as_rule() {
        Rule::strings => Ok(build_ast_from_strings(pair.into_inner().next().unwrap()).unwrap()),
        Rule::bool => todo!(),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

fn build_ast_from_strings(pair: Pair<Rule>) -> IResult<ExprKind> {
    match pair.as_rule() {
        Rule::string => Ok(ExprKind::StringLit(
            build_raw_string_from_string(pair.into_inner().next().unwrap()).unwrap(),
        )),
        Rule::raw_string => Ok(ExprKind::StringLit(StringLit::new(pair.as_str()))),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

fn build_raw_string_from_string(pair: Pair<Rule>) -> IResult<StringLit> {
    Ok(StringLit::new(pair.as_str()))
}

fn build_ast_from_if_expr(pair: Pair<Rule>) -> IResult<ExprKind> {
    // `pair` structure
    // if_expr -> inner {paren_expr, block_expr}
    let mut if_expr = If::place_holder(); // initialize if expr
    for expr in pair.into_inner() {
        match expr.as_rule() {
            // block statement
            Rule::paren_expr => {
                // structured nested expr (paren_expr -> expr)
                let block_expr = build_ast_from_block_expr(expr).unwrap();
                if_expr.add_block(block_expr);
            }
            Rule::brace_stmt => {
                // nested expr (brace stmt -> stmt)
                let stmt = build_ast_from_brace_stmt(expr).unwrap();
                if_expr.add_stmt(stmt);
            }
            Rule::else_if_expr => {
                // recursively apply
                let elif_stmt = build_ast_from_if_expr(expr).unwrap();
                if_expr.add_elif(elif_stmt);
            }
            Rule::else_expr => {
                // nested expr (expr_stmt -> brace stmt -> stmt)

                let else_stmt =
                    build_ast_from_brace_stmt(expr.into_inner().next().unwrap()).unwrap();
                if_expr.add_else(else_stmt);
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    // Check if block and expresions are properly set
    if_expr.check();
    Ok(ExprKind::If(if_expr))
}

fn build_ast_from_block_expr(pair: Pair<Rule>) -> IResult<StmtKind> {
    Ok(build_ast_from_expr(
        pair.into_inner()
            .next()
            .unwrap()
            .into_inner()
            .next()
            .unwrap(),
    )
    .unwrap())
}
fn build_ast_from_brace_stmt(pair: Pair<Rule>) -> IResult<Stmt> {
    Ok(build_ast_from_stmt(
        pair.into_inner()
            .next()
            .unwrap()
            .into_inner()
            .next()
            .unwrap(),
    )
    .unwrap())
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
