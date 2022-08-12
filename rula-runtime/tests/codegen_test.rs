use rula::parser::ast::*;
use rula_runtime::codegen::generator::*;
use std::env;
use std::fs;
use std::io::Write;

fn build_stmt_ast(statement: Stmt) -> AstNode {
    AstNode::RuLa(RuLa::new(RuLaKind::Program(Program::new(
        ProgramKind::Stmt(statement),
    ))))
}

// Helper function to generate
pub fn generate_file(program: RustProgram, file_name: &str) {
    let mut file_path = env::current_dir().unwrap();
    file_path.push("tests");
    file_path.push("generated");
    file_path.push(file_name);
    let mut file = fs::File::create(file_path).unwrap();
    file.write_all(program.program.as_bytes()).unwrap();
}

mod simple_ast_to_rust_tests {
    use super::*;

    #[test]
    fn test_simple() {
        let expected_path = vec![["hello", "world"].iter().collect()];
        let target_ast = vec![build_stmt_ast(Stmt::new(StmtKind::Expr(Expr::new(
            ExprKind::Import(Import::new(PathKind::from(expected_path))),
        ))))];
        let generated_rust = generate(target_ast);
        // generate_file(generated_rust, "/Users/ryosukesatoh/Projects/rula/rula-runtime/tests/generated/test.rs")
        generate_file(generated_rust, "test1.rs")
    }
}
