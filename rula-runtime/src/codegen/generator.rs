// This is entory point to generate code from AST
use super::error::*;
use super::IResult;
use rula::parser::ast::*;

use proc_macro2::TokenStream;

/// Generate corresponding rust code from ast
/// Every nested generators returns piece of TokenStream
pub fn generate(ast_tree: Vec<AstNode>) -> IResult<TokenStream> {
    let mut rula_program = quote!();
    for ast_node in ast_tree {
        match ast_node {
            AstNode::RuLa(rula) => rula_program = generate_rula(rula).unwrap(),
            AstNode::PlaceHolder => {
                return Err(RuLaCompileError::RuLaInitializationError(
                    InitializationError::new("at very first generation"),
                ))
            }
        }
    }
    // let msg = format!("RuLa module that includes all functions");
    let rula_token_stream = quote!(
        // #[doc = #msg]
        mod rula{
            fn main(){
                #rula_program
            }
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
            return Err(RuLaCompileError::RuLaInitializationError(
                InitializationError::new("at generate rula"),
            ))
        }
    }
}

fn generate_program(program: Program) -> IResult<TokenStream> {
    match *program.kind {
        ProgramKind::Stmt(stmt) => Ok(generate_statement(stmt).unwrap()),
    }
}

fn generate_statement(stmt: Stmt) -> IResult<TokenStream> {
    match *stmt.kind {
        StmtKind::Let(let_stmt) => {
            // struct Let {ident, expr}
            let identifier = generate_ident(*let_stmt.ident).unwrap();
            let expr = generate_expr(*let_stmt.expr).unwrap();
            Ok(quote!(
                let mut #identifier = #expr;
            ))
        }
        StmtKind::Expr(expr) => Ok(quote!()),
        StmtKind::PlaceHolder => Err(RuLaCompileError::RuLaInitializationError(
            InitializationError::new("at generate rula"),
        )),
        _ => todo!(),
    }
}
pub fn generate_expr(expr: Expr) -> IResult<TokenStream> {
    Ok(quote!(10))
}

fn generate_ident(ident: Ident) -> IResult<TokenStream> {
    let identifier = format_ident!("{}", *ident.name);
    match *ident.type_hint {
        Some(hint) => {
            let type_hint = generate_type_hint(hint).unwrap();
            Ok(quote!(#identifier:#type_hint))
        }
        None => Ok(quote!(#identifier)),
    }
}

fn generate_type_hint(type_hint: TypeDef) -> IResult<TokenStream> {
    match type_hint {
        TypeDef::Boolean => Ok(quote!(bool)),
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ident tests
    #[test]
    fn test_ident_no_type_hint() {
        let test_ident = Ident::new("hello", None);
        let test_stream = generate_ident(test_ident).unwrap();
        assert_eq!("hello", test_stream.to_string());
    }
    #[test]
    fn test_ident_with_type_hint() {
        let test_ident = Ident::new("hello", Some(TypeDef::Boolean));
        let test_stream = generate_ident(test_ident).unwrap();
        assert_eq!("hello : bool", test_stream.to_string());
    }
}
