// This is entory point to generate code from AST
use super::error::*;
use super::IResult;

use crate::rulep::action::v2::Action as ActionV2;

use crate::rulep::condition::Condition;
use crate::rulep::ruleset::{Rule, RuleSet};
use rula_parser::parser::ast::*;

use once_cell::sync::OnceCell;
use proc_macro2::{Span, TokenStream};
use std::sync::Mutex;
use syn::LitFloat;

// These are used but only in quote! macro
#[allow(unused_imports)]
use mock_components::hardware::result::MeasResult;
#[allow(unused_imports)]
use std::net::SocketAddr;

static RULESET_FORMAT: OnceCell<Mutex<RuleSet<ActionV2>>> = OnceCell::new();
static EVAL_FUNCS: OnceCell<Mutex<Vec<String>>> = OnceCell::new();

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
    // All ast are supposed to be evaluated here
    let rula_token_stream = quote!(
        mod rula{
            #rula_program
        }
        pub fn main(){

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
        ProgramKind::Stmt(stmt) => Ok(generate_stmt(&stmt, None).unwrap()),
        ProgramKind::Interface(interface) => Ok(generate_interface(interface).unwrap()),
    }
}

fn generate_interface(interface: &Interface) -> IResult<TokenStream> {
    let mut interface_names = vec![];
    for i in &interface.interface {
        interface_names.push(generate_ident(i).unwrap())
    }
    let interface_group_name = match &*interface.group_name {
        Some(group_name) => generate_ident(&group_name).unwrap(),
        None => quote!(INTERFACE),
    };

    Ok(quote!(
        use once_cell::sync::OnceCell;
        use std::sync::Mutex;
        use std::collections::HashMap;
        use std::net::IpAddr;
        use mock_components::hardware::qnic::*;

        static #interface_group_name: OnceCell<Mutex<InterfaceGroup>> = OnceCell::new();

        #[derive(Debug)]
        pub struct InterfaceGroup{
            pub interfaces: HashMap<String, QNicInterface>,
        }

        impl InterfaceGroup{
            pub fn new() -> Self{
                InterfaceGroup{interfaces: HashMap::new()}
            }

            pub fn add_interface(&mut self, name: &str, interface: QNicInterface){
                self.interfaces.insert(name.to_string(), interface);
            }
        }

        #[derive(Debug)]
        pub struct QNicInterface{
            pub message_box: HashMap<RuleIdentifier, Message>,
            pub qnic: MockQnic,
        }

        impl QNicInterface{
            pub fn new() -> QNicInterface{
                QNicInterface{
                    message_box: HashMap::new(),
                    qubits: HashMap::new()
                }
            }
            pub fn get_free_qubit() -> QubitInterface{
                /// 0. Look up qubit states
                QubitInterface{qubit_address:10}
            }
        }
        #[derive(Debug)]
        pub struct Message{
            pub socket_addr: SocketAddr,
            pub meas_result: MeasResult,
        }

        #[derive(Debug)]
        pub struct RuleIdentifier{
            pub qnic_address: IpAddr,
            pub rule_id: u32,
        }

        #[derive(Debug)]
        pub struct QubitInterface{
            pub qubit_address: u64,
        }

        pub fn interface_init(){
            let interface = #interface_group_name.get_or_init(|| Mutex::new(InterfaceGroup::new()));
            for inter in vec![#(#interface_names),*]{
                interface.lock().unwrap().add_interface(inter, QNicInterface::new());
            }
        }
    ))
}

fn generate_stmt(stmt: &Stmt, rule: Option<&mut Rule<ActionV2>>) -> IResult<TokenStream> {
    match &*stmt.kind {
        StmtKind::Let(let_stmt) => {
            // struct Let {ident, expr}
            let identifier = generate_ident(&*let_stmt.ident).unwrap();
            let expr = generate_expr(&*let_stmt.expr, None).unwrap();
            Ok(quote!(
                let mut #identifier = #expr;
            ))
        }
        StmtKind::Expr(expr) => Ok(generate_expr(&expr, rule).unwrap()),
        StmtKind::PlaceHolder => Err(RuLaCompileError::RuLaInitializationError(
            InitializationError::new("at generate rula"),
        )),
    }
}
pub fn generate_expr(expr: &Expr, rule: Option<&mut Rule<ActionV2>>) -> IResult<TokenStream> {
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
        ExprKind::RuleSetExpr(ruleset_expr) => Ok(generate_ruleset_expr(&ruleset_expr).unwrap()),
        ExprKind::RuleExpr(rule_expr) => Ok(generate_rule(&rule_expr).unwrap()),
        ExprKind::CondExpr(cond_expr) => Ok(generate_cond(&cond_expr, rule).unwrap()),
        ExprKind::ActExpr(act_expr) => Ok(generate_act(&act_expr, rule).unwrap()),
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
    let block_quote = generate_expr(&*if_expr.block, None).unwrap();
    let stmt_quote = generate_stmt(&*if_expr.stmt, None).unwrap();
    if if_expr.elif.len() > 0 {
        // no elif statement
        if if_expr.elif[0] == None {
            match &*if_expr.els {
                // With else statement
                Some(els_stmt) => {
                    let els_stmt_quote = generate_stmt(&els_stmt, None).unwrap();
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
                    let els_stmt_quote = generate_stmt(&els_stmt, None).unwrap();
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
    let generator = generate_expr(&for_expr.generator, None).unwrap();
    let stmt = generate_stmt(&for_expr.stmt, None).unwrap();
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
    let block = generate_expr(&while_expr.block, None).unwrap();
    let stmt = generate_stmt(&while_expr.stmt, None).unwrap();
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
    let stmt = generate_stmt(&fn_def_expr.stmt, None).unwrap();
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
    let expr = generate_expr(&return_expr.target, None).unwrap();
    Ok(quote!(
        return #expr;
    ))
}
fn generate_comp(comp_expr: &Comp) -> IResult<TokenStream> {
    let lhs = generate_expr(&comp_expr.lhs, None).unwrap();
    let rhs = generate_expr(&comp_expr.rhs, None).unwrap();
    let op = match *comp_expr.comp_op {
        CompOpKind::Lt => quote!(<),
        CompOpKind::Gt => quote!(>),
        CompOpKind::LtE => quote!(<=),
        CompOpKind::GtE => quote!(>=),
        CompOpKind::Eq => quote!(==),
        CompOpKind::Nq => quote!(!=),
        CompOpKind::PlaceHolder => {
            return Err(RuLaCompileError::RuLaInitializationError(
                InitializationError::new("at generating comp expression"),
            ))
        }
    };
    Ok(quote!(#lhs #op #rhs))
}

fn generate_ruleset_expr(ruleset_expr: &RuleSetExpr) -> IResult<TokenStream> {
    // Generate RuleSet
    // if cfg!(feature = "gen-ruleset") {
    // generate portable format with rule information
    let ruleset_name = &*ruleset_expr.name.name;
    let glob_ruleset =
        RULESET_FORMAT.get_or_init(|| Mutex::new(RuleSet::<ActionV2>::new("ruleset")));
    glob_ruleset.lock().unwrap().update_name(ruleset_name);
    // }
    Ok(quote!())
}

fn generate_rule(rule_expr: &RuleExpr) -> IResult<TokenStream> {
    let rule_name = &*rule_expr.name;
    let rule_token = generate_ident(rule_name).unwrap();

    let ruleset = RULESET_FORMAT.get_or_init(|| Mutex::new(RuleSet::<ActionV2>::new("ruleset")));
    let mut rule = Rule::<ActionV2>::new(&rule_name.name);

    // for stmt in &rule_expr.rule_content {
    //     generate_stmt(&stmt, Some(&mut rule)).unwrap();
    // }
    ruleset.lock().unwrap().add_rule(rule);

    Ok(quote!(
        struct #rule_token{
        }
    ))
}

fn generate_cond(cond_expr: &CondExpr, rule: Option<&mut Rule<ActionV2>>) -> IResult<TokenStream> {
    let condition = Condition::new(None);
    for clause in &cond_expr.clauses {}
    match rule {
        Some(rule) => rule.set_condition(condition),
        None => {}
    }
    Ok(quote!())
}

fn generate_act(act_expr: &ActExpr, rule: Option<&mut Rule<ActionV2>>) -> IResult<TokenStream> {
    Ok(quote!())
}
fn generate_array(array_expr: &Array) -> IResult<TokenStream> {
    let mut items = vec![];
    for lits in array_expr.items.iter() {
        items.push(generate_lit(lits).unwrap());
    }
    Ok(quote!(vec![#(#items),*]))
}

fn generate_lit(lit: &Lit) -> IResult<TokenStream> {
    match &*lit.kind {
        LitKind::Ident(ident_lit) => Ok(generate_ident(ident_lit).unwrap()),
        LitKind::NumberLit(number_lit) => {
            let val = LitFloat::new(&*number_lit.value.name, Span::call_site());
            Ok(quote!(#val))
        }
        _ => todo!(),
    }
}

fn generate_term(term_expr: f64) -> IResult<TokenStream> {
    // We could make Term struct instead of direct calc
    // For now, this function just returns calc result as f64
    let val = LitFloat::new(&term_expr.to_string(), Span::call_site());
    Ok(quote!(#val))
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
        let simple_fn_call = FnCall::new(Ident::new("range", None), vec![]);
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

    // Return test
    #[test]
    fn test_simple_return() {
        // return hello
        let simple_return = Return::new(Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(
            Ident::new("hello", None),
        )))));
        let test_stream = generate_return(&simple_return).unwrap();
        assert_eq!("return hello ;", test_stream.to_string());
    }

    // Comp expr test
    #[test]
    fn test_simple_comp() {
        // count > prev_count
        let comp_expr = Comp::new(
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "count", None,
            ))))),
            CompOpKind::Gt,
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "prev_count",
                None,
            ))))),
        );
        let test_stream = generate_comp(&comp_expr).unwrap();
        assert_eq!("count > prev_count", test_stream.to_string());
    }

    #[test]
    fn test_simple_array() {
        // [1, 2, 3, 4, 5]
        let array_expr = Array::new(vec![
            Lit::new(LitKind::NumberLit(NumberLit::new("1"))),
            Lit::new(LitKind::NumberLit(NumberLit::new("2"))),
            Lit::new(LitKind::NumberLit(NumberLit::new("3"))),
            Lit::new(LitKind::NumberLit(NumberLit::new("4"))),
            Lit::new(LitKind::NumberLit(NumberLit::new("5"))),
        ]);
        let test_stream = generate_array(&array_expr).unwrap();
        assert_eq!("vec ! [1 , 2 , 3 , 4 , 5]", test_stream.to_string());
    }

    #[test]
    fn test_simple_rule() {
        // rule hello<qn0>(q2: Qubit){expression}
        // let rule_expr = RuleExpr::new(
        //     Ident::new("hello", None),
        //     vec![Ident::new("qn0", None)],
        //     vec![Ident::new("q2", Some(TypeDef::Qubit))],
        //     vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
        //         Lit::new(LitKind::Ident(Ident::new("expression", None))),
        //     ))))],
        // );
        // let test_stream = generate_rule(&rule_expr).unwrap();
        // assert_eq!(
        //     "rule hello<qn0>(q2: Qubit){expression}",
        //     test_stream.to_string()
        // );
    }

    #[test]
    fn test_interface_generation() {
        // #interface: {qn0, qn1} => qnall;
    }
}
