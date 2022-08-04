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
use ast::TypeDef;
use error::RuLaError;
use std::path::PathBuf;

use once_cell::sync::Lazy;
use pest::iterators::Pair;
use pest::prec_climber::{Assoc, Operator, PrecClimber};

// Custome error interface for rula
pub type IResult<T> = std::result::Result<T, RuLaError>;

static PREC_CLIMBER: Lazy<PrecClimber<Rule>> = Lazy::new(|| {
    PrecClimber::new(vec![
        Operator::new(Rule::plus, Assoc::Left) | Operator::new(Rule::minus, Assoc::Left),
        Operator::new(Rule::asterisk, Assoc::Left) | Operator::new(Rule::slash, Assoc::Left),
        Operator::new(Rule::caret, Assoc::Right),
    ])
});

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
    let mut let_stmt = Let::place_holder();
    for let_pair in pair.into_inner() {
        match let_pair.as_rule() {
            Rule::ident => {
                let_stmt.add_ident(build_ast_from_ident(let_pair).unwrap());
            }
            Rule::ident_typed => {
                let_stmt.add_ident(build_ast_from_ident_typed(let_pair).unwrap());
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

fn build_ast_from_ident_typed(pair: Pair<Rule>) -> IResult<Ident> {
    let mut identifier = Ident::place_holder();
    for ids in pair.into_inner() {
        match ids.as_rule() {
            Rule::ident => {
                identifier = build_ast_from_ident(ids).unwrap();
            }
            Rule::typedef_lit => {
                identifier.add_type_hint(Some(
                    build_ast_from_typedef_lit(ids.into_inner().next().unwrap()).unwrap(),
                ));
            }
            _ => unreachable!(),
        }
    }
    Ok(identifier)
}

// Parse identifier(Endpoint)
fn build_ast_from_ident(pair: Pair<Rule>) -> IResult<Ident> {
    let mut identifier = Ident::place_holder();
    match pair.as_rule() {
        Rule::ident => {
            identifier.add_name(pair.as_str());
        }
        _ => return Err(RuLaError::RuLaSyntaxError),
    }
    Ok(identifier)
}

// Parse expression <--> {If | Import | Lit | Identifier}
fn build_ast_from_expr(pair: Pair<Rule>) -> IResult<Expr> {
    match pair.as_rule() {
        // Same order as .pest file
        // import hello::world;
        Rule::import_expr => Ok(Expr::new(ExprKind::Import(
            build_ast_from_import_expr(pair).unwrap(),
        ))),
        // fn(hello:i32){expression;};
        Rule::fn_def_expr => Ok(Expr::new(ExprKind::FnDef(
            build_ast_from_fn_def_expr(pair).unwrap(),
        ))),
        // if(block){expression;};
        Rule::if_expr => Ok(Expr::new(ExprKind::If(
            build_ast_from_if_expr(pair).unwrap(),
        ))),
        // 1+10, (1+18)*20
        // Rule::term => Ok(Expr::new(ExprKind::Term(eval_term(pair.into_inner())))),
        Rule::term => Ok(Expr::new(ExprKind::Term(eval_prec(pair)))),
        // true, false, "words"
        Rule::literals => Ok(Expr::new(ExprKind::Lit(
            build_ast_from_literals(pair.into_inner().next().unwrap()).unwrap(),
        ))),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

// Don't want to evaluate anything at this moment.
fn eval_prec(pair: Pair<Rule>) -> f64 {
    // primary closure taking pair and throw it to consume function.
    let primary = |pair| eval_prec(pair);
    let infix = |lhs: f64, op: Pair<Rule>, rhs: f64| match op.as_rule() {
        Rule::plus => lhs + rhs,
        Rule::minus => lhs - rhs,
        Rule::asterisk => lhs * rhs,
        Rule::slash => lhs / rhs,
        Rule::caret => lhs.powf(rhs),
        _ => unreachable!("operation unreachable, {:#?}", &op),
    };

    match pair.as_rule() {
        Rule::term => PREC_CLIMBER.climb(pair.into_inner(), primary, infix),
        Rule::number => pair.as_str().parse().unwrap(),
        _ => unreachable!("unreachable{:#?}", &pair),
    }
}

// Parse Literals <--> {string literal | boolean literal}
fn build_ast_from_literals(pair: Pair<Rule>) -> IResult<Lit> {
    match pair.as_rule() {
        // identifier
        Rule::ident => Ok(Lit::new(LitKind::Ident(
            build_ast_from_ident(pair).unwrap(),
        ))),
        Rule::raw_string => Ok(Lit::new(LitKind::StringLit(StringLit::new(pair.as_str())))),
        Rule::bool => match pair.as_str() {
            "true" => Ok(Lit::new(LitKind::BooleanLit(true))),
            "false" => Ok(Lit::new(LitKind::BooleanLit(false))),
            _ => unreachable!(),
        },
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
            Rule::expr => {
                if_expr.add_block(build_ast_from_expr(expr.into_inner().next().unwrap()).unwrap());
            }
            Rule::compr => {
                todo!()
            }
            Rule::stmt => {
                // nested expr (brace stmt -> stmt)
                if_expr.add_stmt(build_ast_from_stmt(expr.into_inner().next().unwrap()).unwrap());
            }
            Rule::else_if_expr => {
                // recursively apply
                if_expr.add_elif(build_ast_from_if_expr(expr).unwrap());
            }
            Rule::else_expr => {
                let else_stmt = build_ast_from_stmt(
                    expr.into_inner()
                        .next()
                        .unwrap()
                        .into_inner()
                        .next()
                        .unwrap(),
                )
                .unwrap();
                if_expr.add_else(else_stmt);
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    // Check if block and expresions are properly set
    if_expr.check();
    Ok(if_expr)
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
                    fnc_def.add_arg(build_ast_from_ident_typed(arg).unwrap());
                }
            }
            Rule::stmt => {
                fnc_def.add_expr(build_ast_from_stmt(block.into_inner().next().unwrap()).unwrap())
            }
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
