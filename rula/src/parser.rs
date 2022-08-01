pub mod ast;
pub mod error;
pub mod token;
mod util;

use crate::Rule;
// RuLa
use ast::{RuLa, RuLaKind};
// Program
use ast::{Program, ProgramKind};
// Statements
use ast::{Let, Stmt, StmtKind};
// Expressions
use ast::{Expr, ExprKind, FnDef, Ident, If, Import, Lit, LitKind, PathKind, StringLit};
// Literals
use ast::{IntegerLit, TypeDef};
use error::RuLaError;
use std::path::PathBuf;

use pest::iterators::Pair;

// Custome error interface for rula
pub type IResult<T> = std::result::Result<T, RuLaError>;

/**
 * parse rula system and if it matches program, going to `build_ast_from_program`.
*/
pub fn build_ast_from_rula(pair: Pair<Rule>) -> IResult<RuLa> {
    match pair.as_rule() {
        Rule::program => {
            if pair.as_str() == "" {
                // edge case (e.g. only comment)
                return Ok(RuLa::ignore());
            } else {
                return Ok(RuLa::new(RuLaKind::Program(
                    build_ast_from_program(pair.into_inner().next().unwrap()).unwrap(),
                )));
            }
        }
        Rule::EOI => Ok(RuLa::aoi()),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

// Parse program <-> statement
fn build_ast_from_program(pair: Pair<Rule>) -> IResult<Program> {
    match pair.as_rule() {
        Rule::stmt => Ok(Program::new(ProgramKind::Stmt(
            build_ast_from_stmt(pair.into_inner().next().unwrap()).unwrap(),
        ))),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

// Parse statement <--> {Let statement | expression}
fn build_ast_from_stmt(pair: Pair<Rule>) -> IResult<Stmt> {
    match pair.as_rule() {
        Rule::let_stmt => Ok(Stmt::new(StmtKind::Let(
            build_ast_from_let_stmt(pair).unwrap(),
        ))),
        Rule::expr => Ok(Stmt::new(StmtKind::Expr(
            build_ast_from_expr(pair.into_inner().next().unwrap()).unwrap(),
        ))),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

// Parse Let statement (semi endpoint)
fn build_ast_from_let_stmt(pair: Pair<Rule>) -> IResult<Let> {
    // Ugh ugly
    let mut let_stmt = Let::place_holder();
    for let_pair in pair.into_inner() {
        match let_pair.as_rule() {
            Rule::ident => {
                let_stmt.add_ident(build_ast_from_ident(let_pair).unwrap());
            }
            Rule::expr => {
                let_stmt
                    .add_expr(build_ast_from_expr(let_pair.into_inner().next().unwrap()).unwrap());
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(let_stmt)
}

// Parse identifier(Endpoint)
fn build_ast_from_ident(pair: Pair<Rule>) -> IResult<Ident> {
    let mut identifier = Ident::place_holder();
    for ids in pair.into_inner() {
        match ids.as_rule() {
            Rule::identifier => {
                identifier.add_name(ids.as_str());
            }
            Rule::typedef_lit => {
                identifier.add_type_hint(Some(
                    build_ast_from_typedef_lit(ids.into_inner().next().unwrap()).unwrap(),
                ));
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    identifier.check();
    Ok(identifier)
}

// Parse expression <--> {If | Import | Lit | Identifier}
fn build_ast_from_expr(pair: Pair<Rule>) -> IResult<Expr> {
    match pair.as_rule() {
        Rule::if_expr => Ok(Expr::new(ExprKind::If(
            build_ast_from_if_expr(pair).unwrap(),
        ))),
        Rule::import_expr => Ok(Expr::new(ExprKind::Import(
            build_ast_from_import_expr(pair).unwrap(),
        ))),
        Rule::fn_def_expr => Ok(Expr::new(ExprKind::FnDef(
            build_ast_from_fn_def_expr(pair).unwrap(),
        ))),
        Rule::literals => Ok(Expr::new(ExprKind::Lit(
            build_ast_from_literals(pair.into_inner().next().unwrap()).unwrap(),
        ))),
        Rule::ident => Ok(Expr::new(ExprKind::Ident(
            build_ast_from_ident(pair).unwrap(),
        ))),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

// Parse Literals <--> {string literal | boolean literal}
fn build_ast_from_literals(pair: Pair<Rule>) -> IResult<Lit> {
    match pair.as_rule() {
        Rule::strings => Ok(Lit::new(LitKind::StringLit(
            build_ast_from_strings(pair.into_inner().next().unwrap()).unwrap(),
        ))),
        Rule::number => Ok(Lit::new(LitKind::IntegerLit(
            build_ast_from_number(pair.into_inner().next().unwrap()).unwrap(),
        ))),
        Rule::bool => todo!(),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

// Parse string literal <--> {string | raw string} semi endpoint
fn build_ast_from_strings(pair: Pair<Rule>) -> IResult<StringLit> {
    match pair.as_rule() {
        Rule::string => {
            Ok(build_raw_string_from_string(pair.into_inner().next().unwrap()).unwrap())
        }
        Rule::raw_string => Ok(StringLit::new(pair.as_str())),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

// Parse raw_string literal endpoint
fn build_raw_string_from_string(pair: Pair<Rule>) -> IResult<StringLit> {
    Ok(StringLit::new(pair.as_str()))
}

fn build_ast_from_number(pair: Pair<Rule>) -> IResult<IntegerLit> {
    match pair.as_rule() {
        Rule::int => Ok(IntegerLit::new(pair.as_str())),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

// Parse If expr <--> {paren_expr | brace_stmt | else_if | else} semi endpoint
fn build_ast_from_if_expr(pair: Pair<Rule>) -> IResult<If> {
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
    Ok(if_expr)
}

// parse block expression <--> expr
fn build_ast_from_block_expr(pair: Pair<Rule>) -> IResult<Expr> {
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

fn build_ast_from_import_expr(pair: Pair<Rule>) -> IResult<Import> {
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

    Ok(Import::new(PathKind::from(path_list)))
}

fn build_ast_from_fn_def_expr(pair: Pair<Rule>) -> IResult<FnDef> {
    let mut fnc_def = FnDef::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::arguments => {
                // loop over all arguments
                for arg in block.into_inner() {
                    fnc_def.add_arg(build_ast_from_ident(arg).unwrap());
                }
            }
            Rule::brace_stmt => fnc_def.add_expr(build_ast_from_brace_stmt(block).unwrap()),
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(fnc_def)
}

fn build_ast_from_typedef_lit(pair: Pair<Rule>) -> IResult<TypeDef> {
    match pair.as_rule() {
        Rule::integer_type => match pair.as_str() {
            "i32" => return Ok(TypeDef::Integer32),
            "i64" => return Ok(TypeDef::Integer64),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        Rule::unsigned_integer_type => match pair.as_str() {
            "u32" => return Ok(TypeDef::UnsignedInteger32),
            "u64" => return Ok(TypeDef::UnsignedInteger64),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        Rule::float_type => match pair.as_str() {
            "f32" => return Ok(TypeDef::Float32),
            "f64" => return Ok(TypeDef::Float64),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        Rule::complex_type => match pair.as_str() {
            "c64" => return Ok(TypeDef::Complex64),
            "c128" => return Ok(TypeDef::Complex128),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        Rule::boolean_type => match pair.as_str() {
            "bool" => return Ok(TypeDef::Boolean),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        Rule::string_type => match pair.as_str() {
            "str" => return Ok(TypeDef::Str),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        Rule::qubit_type => match pair.as_str() {
            "qubit" => return Ok(TypeDef::Qubit),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        _ => todo!("Should be unknown type error here"),
    }
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
