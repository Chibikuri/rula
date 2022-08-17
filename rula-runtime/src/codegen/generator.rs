// This is entory point to generate code from AST
use super::error::*;
use super::IResult;
use rula::parser::ast::*;

use proc_macro2::{Span, TokenStream};

/// Generate corresponding rust code from ast
/// Every nested generators returns piece of TokenStream
pub fn generate(ast_tree: Vec<AstNode>) -> IResult<TokenStream> {
    let mut rula_program = quote!();
    for ast_node in ast_tree {
        match ast_node {
            AstNode::RuLa(rula) => rula_program = generate_rula(&rula).unwrap(),
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

fn generate_rula(rula: &RuLa) -> IResult<TokenStream> {
    match &*rula.rula {
        RuLaKind::Program(program) => {
            return Ok(generate_program(&program).unwrap());
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

fn generate_program(program: &Program) -> IResult<TokenStream> {
    match &*program.kind {
        ProgramKind::Stmt(stmt) => Ok(generate_stmt(&stmt).unwrap()),
    }
}

fn generate_stmt(stmt: &Stmt) -> IResult<TokenStream> {
    match &*stmt.kind {
        StmtKind::Let(let_stmt) => {
            // struct Let {ident, expr}
            let identifier = generate_ident(&*let_stmt.ident).unwrap();
            let expr = generate_expr(&*let_stmt.expr).unwrap();
            Ok(quote!(
                let mut #identifier = #expr;
            ))
        }
        StmtKind::Expr(expr) => Ok(generate_expr(&expr).unwrap()),
        StmtKind::PlaceHolder => Err(RuLaCompileError::RuLaInitializationError(
            InitializationError::new("at generate rula"),
        )),
    }
}
pub fn generate_expr(expr: &Expr) -> IResult<TokenStream> {
    match &*expr.kind {
        ExprKind::Import(import_expr) => Ok(generate_import(&import_expr).unwrap()),
        ExprKind::If(if_expr) => Ok(generate_if(&if_expr).unwrap()),
        ExprKind::For(for_expr) => Ok(quote!()),
        ExprKind::While(while_expr) => Ok(quote!()),
        ExprKind::FnDef(fn_def_expr) => Ok(quote!()),
        ExprKind::FnCall(fn_call_expr) => Ok(quote!()),
        ExprKind::Struct(struct_expr) => Ok(quote!()),
        ExprKind::Return(return_expr) => Ok(quote!()),
        ExprKind::Comp(comp_expr) => Ok(quote!()),
        ExprKind::RuleExpr(rule_expr) => Ok(quote!()),
        ExprKind::CondExpr(cond_expr) => Ok(quote!()),
        ExprKind::ActExpr(act_expr) => Ok(quote!()),
        ExprKind::Array(array_expr) => Ok(quote!()),
        ExprKind::Lit(lit_expr) => Ok(generate_lit(&lit_expr).unwrap()),
        ExprKind::Term(term_expr) => Ok(quote!()),
        ExprKind::PlaceHolder => Err(RuLaCompileError::RuLaInitializationError(
            InitializationError::new("at generating expression"),
        )),
    }
}

fn generate_import(import_expr: &Import) -> IResult<TokenStream> {
    // Convert a set of paths into a set of identifiers
    let path = import_expr.path.convert_to_ident();
    // Vector to store all paths
    // e.g. [hello::world, good::evening::Yall]
    let mut paths = vec![];
    for path_ident in path.iter() {
        let mut single_path = vec![];
        let quoted_path = if path_ident.name.contains("/") {
            let splitted = path_ident.name.split("/");
            // This is not clean
            for sp in splitted.into_iter() {
                let new_ident = Ident::new(sp, None);
                let path_fragment = generate_ident(&new_ident).unwrap();
                single_path.push(path_fragment)
            }
            let path_head = &single_path[0];
            let path_left = &single_path[1..];
            quote!(
                #path_head #(::#path_left)*
            )
        } else {
            let path_ident = generate_ident(path_ident).unwrap();
            quote!(#path_ident)
        };
        paths.push(quoted_path);
    }
    Ok(quote!(
        #(use #paths; )*
    ))
}

#[allow(unused)]
fn generate_if(if_expr: &If) -> IResult<TokenStream> {
    // match &*if_expr.block.kind {
    //     // At this point, if block cannot be limited
    //     ExprKind::Comp(comp_expr) => {
    //         block_quote = generate_comp(&comp_expr).unwrap();
    //     }
    //     _ => {
    //         return Err(RuLaCompileError::RuLaInitializationError(
    //             InitializationError::new("at generate if"),
    //         ))
    //     }
    // }
    // We could limit the available expression
    let block_quote = generate_expr(&*if_expr.block).unwrap();
    let stmt_quote = generate_stmt(&*if_expr.stmt).unwrap();
    if if_expr.elif.len() > 0 {
        // no elif statement
        if if_expr.elif[0] == None {
            match &*if_expr.els {
                // With else statement
                Some(els_stmt) => {
                    let els_stmt_quote = generate_stmt(&els_stmt).unwrap();
                    return Ok(quote!(
                        if #block_quote{
                            #stmt_quote
                        }else{
                            #els_stmt_quote
                        }
                    ));
                }
                None => {
                    // No error statement
                    // This could be error in rust code
                    return Ok(quote!(
                        if #block_quote{
                            #stmt_quote
                        }
                    ));
                }
            }
        } else {
            // No elif expr
            let mut elif_quotes = vec![];
            for elif_expr in &*if_expr.elif {
                match elif_expr {
                    Some(if_expr) => elif_quotes.push(generate_if(&if_expr).unwrap()),
                    None => {
                        unreachable!()
                    }
                }
            }

            match &*if_expr.els {
                Some(els_stmt) => {
                    let els_stmt_quote = generate_stmt(&els_stmt).unwrap();
                    Ok(quote!(
                        if #block_quote {
                            #stmt_quote
                        }#(elif #elif_quotes)*

                        else{
                            #els_stmt_quote
                        }
                    ))
                }
                None => Ok(quote!(
                    if #block_quote {
                        #stmt_quote
                    }#(elif #elif_quotes)*
                )),
            }
        }
    } else {
        // Duplication?
        panic!("This should not happen in the current initialization");
    }
}

fn generate_comp(comp_expr: &Comp) -> IResult<TokenStream> {
    Ok(quote!())
}

fn generate_lit(lit: &Lit) -> IResult<TokenStream> {
    match &*lit.kind {
        LitKind::Ident(ident_lit) => Ok(generate_ident(ident_lit).unwrap()),
        _ => todo!(),
    }
}
// Generate identifier token from Ident ast
fn generate_ident(ident: &Ident) -> IResult<TokenStream> {
    let identifier = format_ident!("{}", *ident.name);
    match &*ident.type_hint {
        Some(hint) => {
            let type_hint = generate_type_hint(hint).unwrap();
            Ok(quote!(#identifier:#type_hint))
        }
        None => Ok(quote!(#identifier)),
    }
}

fn generate_type_hint(type_hint: &TypeDef) -> IResult<TokenStream> {
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
        let test_stream = generate_ident(&test_ident).unwrap();
        assert_eq!("hello", test_stream.to_string());
    }
    #[test]
    fn test_ident_with_type_hint() {
        let test_ident = Ident::new("hello", Some(TypeDef::Boolean));
        let test_stream = generate_ident(&test_ident).unwrap();
        assert_eq!("hello : bool", test_stream.to_string());
    }

    // Import test
    #[test]
    fn test_simple_import() {
        // import hello;
        let expected_paths = vec![["hello"].iter().collect()];
        let test_import = Import::new(PathKind::from(expected_paths));
        let test_stream = generate_import(&test_import).unwrap();
        assert_eq!("use hello ;", test_stream.to_string());
    }

    // If test
    #[test]
    fn test_simple_if() {
        // if (block) {expression}
        let simple_if = If::new(
            // (block)
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "block", None,
            ))))),
            // {expression}
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("expression", None)),
            ))))),
            // elif ~
            None,
            // else ~
            None,
        );
        let test_stream = generate_if(&simple_if).unwrap();
        assert_eq!("if block { expression }", test_stream.to_string());
    }
}
