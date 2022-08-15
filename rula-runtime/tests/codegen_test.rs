use proc_macro2::TokenStream;
use rula::parser::ast::*;
use rula_runtime::codegen::generator::*;
use std::env;
use std::fs;
use std::io::Write;
use std::process::Command;

fn build_stmt_ast(statement: Stmt) -> AstNode {
    AstNode::RuLa(RuLa::new(RuLaKind::Program(Program::new(
        ProgramKind::Stmt(statement),
    ))))
}

// Helper function to generate
pub fn generate_file(program: TokenStream, file_name: &str) {
    let mut file_path = env::current_dir().unwrap();
    file_path.push("tests");
    file_path.push("generated");
    file_path.push(file_name);
    let mut file = fs::File::create(file_path.clone()).unwrap();
    writeln!(file, "// This is autogenerated Rust program \n{}", program).unwrap();
    // Format generated file
    #[cfg(not(feature = "no-format"))]
    Command::new("cargo")
        .arg("fmt")
        .arg("--")
        .arg(file_path)
        .spawn()
        .expect("Command failed");
}

mod simple_ast_to_rust_tests {
    use super::*;

    #[test]
    fn test_simple() {
        let expected_path = vec![["hello", "world"].iter().collect()];
        let target_ast = vec![build_stmt_ast(Stmt::new(StmtKind::Expr(Expr::new(
            ExprKind::Import(Import::new(PathKind::from(expected_path))),
        ))))];
        let generated_rust = generate(target_ast).unwrap();
        #[cfg(not(feature = "ci"))]
        generate_file(generated_rust, "test1.rs")
    }

    #[test]
    fn test_simple_let() {
        // let hello = world;
        let target_ast = vec![build_stmt_ast(Stmt::new(StmtKind::Let(Let::new(
            Ident::new("hello", None),
            Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                "world", None,
            ))))),
        ))))];
        let generated_rust = generate(target_ast).unwrap();
        #[cfg(not(feature = "ci"))]
        generate_file(generated_rust, "test2.rs")
    }
}
