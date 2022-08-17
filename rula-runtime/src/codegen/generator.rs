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
        ExprKind::For(for_expr) => Ok(generate_for(&for_expr).unwrap()),
        ExprKind::While(while_expr) => Ok(generate_while(&while_expr).unwrap()),
        ExprKind::FnDef(fn_def_expr) => Ok(generate_fn_def(&fn_def_expr).unwrap()),
        ExprKind::FnCall(fn_call_expr) => Ok(generate_fn_call(&fn_call_expr).unwrap()),
        ExprKind::Struct(struct_expr) => Ok(generate_struct(&struct_expr).unwrap()),
        ExprKind::Return(return_expr) => Ok(generate_return(&return_expr).unwrap()),
        ExprKind::Comp(comp_expr) => Ok(generate_comp(&comp_expr).unwrap()),
        ExprKind::RuleExpr(rule_expr) => Ok(generate_rule(&rule_expr).unwrap()),
        ExprKind::CondExpr(cond_expr) => Ok(generate_cond(&cond_expr).unwrap()),
        ExprKind::ActExpr(act_expr) => Ok(generate_act(&act_expr).unwrap()),
        ExprKind::Array(array_expr) => Ok(generate_array(&array_expr).unwrap()),
        ExprKind::Lit(lit_expr) => Ok(generate_lit(&lit_expr).unwrap()),
        ExprKind::Term(term_expr) => Ok(generate_term(*term_expr).unwrap()),
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
    // block could have invalid expression here.
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
                        }#(else #elif_quotes)*

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

fn generate_for(for_expr: &For) -> IResult<TokenStream> {
    let mut ident_list = vec![];
    for ident in for_expr.pattern.iter() {
        ident_list.push(generate_ident(ident).unwrap());
    }
    let generator = generate_expr(&for_expr.generator).unwrap();
    let stmt = generate_stmt(&for_expr.stmt).unwrap();
    if ident_list.len() == 1 {
        let var = &ident_list[0];
        Ok(quote!(
            for #var in #generator {
                #stmt
            }
        ))
    } else if ident_list.len() > 1 {
        Ok(quote!(
            for (#(#ident_list),* ) in generator {
                #stmt
            }
        ))
    } else {
        Err(RuLaCompileError::RuLaInitializationError(
            InitializationError::new("at generating for expression"),
        ))
    }
}

fn generate_while(while_expr: &While) -> IResult<TokenStream> {
    let block = generate_expr(&while_expr.block).unwrap();
    let stmt = generate_stmt(&while_expr.stmt).unwrap();
    Ok(quote!(
        while #block{
            #stmt
        }
    ))
}

fn generate_fn_def(fn_def_expr: &FnDef) -> IResult<TokenStream> {
    let mut arguments = vec![];
    for ident in fn_def_expr.arguments.iter() {
        arguments.push(generate_ident(ident).unwrap());
    }
    let stmt = generate_stmt(&fn_def_expr.stmt).unwrap();
    // Here could have generics in the future
    Ok(quote!(
        fn ( #(#arguments),* ) {
            #stmt
        }
    ))
}
fn generate_fn_call(fn_call_expr: &FnCall) -> IResult<TokenStream> {
    let fn_name = generate_ident(&fn_call_expr.func_name).unwrap();
    Ok(quote!(
        #fn_name ()
    ))
}
fn generate_struct(struct_expr: &Struct) -> IResult<TokenStream> {
    let struct_name = generate_ident(&struct_expr.name).unwrap();
    let mut struct_items = vec![];
    for ident in struct_expr.items.iter() {
        struct_items.push(generate_ident(ident).unwrap());
    }
    Ok(quote!(
        struct #struct_name{
            #(#struct_items),*
        }
    ))
}
fn generate_return(return_expr: &Return) -> IResult<TokenStream> {
    Ok(quote!())
}
fn generate_comp(comp_expr: &Comp) -> IResult<TokenStream> {
    Ok(quote!())
}
fn generate_rule(rule_expr: &RuleExpr) -> IResult<TokenStream> {
    Ok(quote!())
}
fn generate_cond(cond_expr: &CondExpr) -> IResult<TokenStream> {
    Ok(quote!())
}

fn generate_act(act_expr: &ActExpr) -> IResult<TokenStream> {
    Ok(quote!())
}
fn generate_array(array_expr: &Array) -> IResult<TokenStream> {
    Ok(quote!())
}

fn generate_lit(lit: &Lit) -> IResult<TokenStream> {
    match &*lit.kind {
        LitKind::Ident(ident_lit) => Ok(generate_ident(ident_lit).unwrap()),
        _ => todo!(),
    }
}

fn generate_term(term_expr: f64) -> IResult<TokenStream> {
    // We could make Term struct instead of direct calc
    Ok(quote!())
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
        TypeDef::Integer32 => Ok(quote!(i32)),
        TypeDef::Integer64 => Ok(quote!(i64)),
        TypeDef::Str => Ok(quote!(String)),
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

    #[test]
    fn test_if_else() {
        // if (block) {expression} else {expression2}
        let if_else = If::new(
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
            Some(Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
                Lit::new(LitKind::Ident(Ident::new("expression2", None))),
            ))))),
        );
        let test_stream = generate_if(&if_else).unwrap();
        assert_eq!(
            "if block { expression } else { expression2 }",
            test_stream.to_string()
        );
    }

    #[test]
    fn test_if_elif_else() {
        // if(block){expression} else if (block2){expression2} else {expression3}
        let if_elif_else = If::new(
            // (block)
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "block", None,
            ))))),
            // {expression}
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("expression", None)),
            ))))),
            // elif ~
            Some(If::new(
                // else if (block)
                Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                    "block2", None,
                ))))),
                // else if () {statement2;};
                Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                    LitKind::Ident(Ident::new("expression2", None)),
                ))))),
                None,
                None,
            )),
            // else ~
            Some(Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
                Lit::new(LitKind::Ident(Ident::new("expression3", None))),
            ))))),
        );
        let test_stream = generate_if(&if_elif_else).unwrap();
        assert_eq!(
            "if block { expression } else if block2 { expression2 } else { expression3 }",
            test_stream.to_string()
        );
    }

    // for test
    #[test]
    fn test_simple_for_generation() {
        // for (i) in generator {hello}
        let simple_for = For::new(
            vec![Ident::new("i", None)],
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "generator",
                None,
            ))))),
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("hello", None)),
            ))))),
        );
        let test_stream = generate_for(&simple_for).unwrap();
        assert_eq!("for i in generator { hello }", test_stream.to_string());
    }

    #[test]
    fn test_multi_arg_for_generation() {
        // for (a, b, c) in generator{hello}
        let multi_for = For::new(
            vec![
                Ident::new("a", None),
                Ident::new("b", None),
                Ident::new("c", None),
            ],
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "generator",
                None,
            ))))),
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("hello", None)),
            ))))),
        );
        let test_stream = generate_for(&multi_for).unwrap();
        assert_eq!(
            "for (a , b , c) in generator { hello }",
            test_stream.to_string()
        );
    }
    // While test
    #[test]
    fn test_simple_while() {
        let simple_while = While::new(
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "count", None,
            ))))),
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("expression", None)),
            ))))),
        );
        let test_stream = generate_while(&simple_while).unwrap();
        assert_eq!("while count { expression }", test_stream.to_string());
    }

    // FnDef test
    #[test]
    fn test_simple_fn_def() {
        // fn(block:i32, hello:str){expression}
        let simple_fn_def = FnDef::new(
            vec![
                Ident::new("block", Some(TypeDef::Integer32)),
                Ident::new("hello", Some(TypeDef::Str)),
            ],
            Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(Ident::new("expression", None)),
            ))))),
        );
        let test_stream = generate_fn_def(&simple_fn_def).unwrap();
        assert_eq!(
            "fn (block : i32 , hello : String) { expression }",
            test_stream.to_string()
        );
    }

    // FnCall test
    #[test]
    fn test_simple_fn_call() {
        // range()
        let simple_fn_call = FnCall::new(Ident::new("range", None));
        let test_stream = generate_fn_call(&simple_fn_call).unwrap();
        assert_eq!("range ()", test_stream.to_string());
    }

    // Struct test
    #[test]
    fn test_simple_struct() {
        // struct Test{flag: bool}
        let simple_struct = Struct::new(
            Ident::new("Test", None),
            vec![Ident::new("flag", Some(TypeDef::Boolean))],
        );
        let test_stream = generate_struct(&simple_struct).unwrap();
        assert_eq!("struct Test { flag : bool }", test_stream.to_string());
    }
}
