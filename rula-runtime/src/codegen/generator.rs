// This is entory point to generate code from AST
use super::error::*;
use super::program::generate_program;
use rula::parser::ast::*;

use proc_macro2::TokenStream;
use std::collections::HashMap;
use std::marker::PhantomData;
use syn::{parse_quote, ItemMod};

pub type IResult<T> = std::result::Result<T, RuLaCompileError>;

/// Generate corresponding rust code from ast
/// Every nested generators returns piece of TokenStream
pub fn generate(ast_tree: Vec<AstNode>) -> IResult<TokenStream> {
    let mut rula_program = quote!();
    for ast_node in ast_tree {
        match ast_node {
            AstNode::RuLa(rula) => rula_program = generate_rula(rula).unwrap(),
            AstNode::PlaceHolder => {
                return Err(RuLaCompileError::RuLaInitializationError(
                    InitializationError::new("at generate function"),
                ))
            }
        }
    }
    let msg = format!("RuLa module that includes all functions");
    let rula_token_stream = quote!(
        #[doc = #msg]
        mod rula{
            #rula_program
        }
    );
    Ok(rula_token_stream)
}

fn generate_rula(rula: RuLa) -> IResult<TokenStream> {
    match *rula.rula {
        RuLaKind::Program(program) => {
            return Ok(generate_program(program).unwrap());
        }
        RuLaKind::Ignore => return Ok(quote!()),
        RuLaKind::Eoi => {
            return Ok(quote!(
                #[doc = "End of input"]
            ))
        }
        RuLaKind::PlaceHolder => {
            panic!("Error: value not properly set")
        }
    }
}
