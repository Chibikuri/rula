use super::generator::*;
use proc_macro2::TokenStream;
use rula::parser::ast::*;

pub fn generate_program(program: Program) -> IResult<TokenStream> {
    match *program.kind {
        ProgramKind::Stmt(stmt) => {
            let func_def = quote!(
                fn hello() {
                    println!("Hello world!");
                }
            );
            Ok(quote!(
                    #func_def
            ))
        }
    }
    // Ok(program)
}
