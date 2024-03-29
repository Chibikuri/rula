// use rula_exec::codegen::generator::*;
// use rula_parser::parser::ast::*;

// use proc_macro2::TokenStream;
// use std::env;
// use std::fs;
// use std::io::Write;
// use std::process::Command;

// // Helper macro to return function name
// macro_rules! test_name {
//     () => {{
//         fn f() {}
//         fn type_name_of<T>(_: T) -> String {
//             let mut mod_name = std::any::type_name::<T>()
//                 .split("::")
//                 .into_iter()
//                 .nth(1)
//                 .unwrap()
//                 .to_owned();
//             let file_name = std::any::type_name::<T>()
//                 .split("::")
//                 .into_iter()
//                 .nth(2)
//                 .unwrap();
//             mod_name.push_str("::");
//             mod_name.push_str(file_name);
//             mod_name
//         }
//         &(type_name_of(f) + ".rs")
//     }};
// }

// // Helper function to generate file
// pub fn generate_token_stream_file(program: TokenStream, file_name: &str) {
//     let mut file_path = env::current_dir().unwrap();
//     file_path.push("tests");
//     file_path.push("generated");
//     file_path.push(file_name);
//     let mut file = fs::File::create(file_path.clone()).unwrap();
//     writeln!(file, "// This is autogenerated Rust program \n{}", program).unwrap();
//     // Format generated file
//     #[cfg(not(feature = "no-format"))]
//     Command::new("cargo")
//         .arg("fmt")
//         .arg("--")
//         .arg(file_path)
//         .spawn()
//         .expect("Command failed");
// }

// pub fn get_correct_file_tokens(file_name: &str) -> String {
//     let mut file_path = env::current_dir().unwrap();
//     file_path.push("tests");
//     file_path.push("suppose");
//     file_path.push(file_name);
//     let contents = fs::read_to_string(file_path).expect("Failed to read the file");
//     contents.to_string()
// }

// fn build_stmt_ast(statement: Stmt) -> AstNode {
//     AstNode::RuLa(RuLa::new(RuLaKind::Program(Program::new(vec![
//         ProgramKind::Stmt(statement),
//     ]))))
// }

// mod import_ast_to_rust_tests {
//     use super::*;

//     #[test]
//     #[ignore = "Temporary"]
//     fn test_simple_import_single() {
//         // import hello;
//         let expected_path = vec![["hello"].iter().collect()];
//         let mut target_ast = build_stmt_ast(Stmt::new(StmtKind::Expr(Expr::new(
//             ExprKind::Import(Import::new(PathKind::from(expected_path))),
//         ))));
//         let generated_rust = generate(&mut target_ast, false, None).unwrap();
//         let target_rust = "use rula_lib as rula_std ; # [allow (unused)] mod rula { use super :: * ; use hello ; } pub fn main () { }";
//         assert_eq!(generated_rust.to_string(), target_rust);
//         #[cfg(not(feature = "ci"))]
//         generate_token_stream_file(generated_rust, test_name!())
//     }

//     #[test]
//     #[ignore = "Temporary"]
//     fn test_simple_import() {
//         // import hello::world;
//         let expected_path = vec![["hello", "world"].iter().collect()];
//         let mut target_ast = build_stmt_ast(Stmt::new(StmtKind::Expr(Expr::new(
//             ExprKind::Import(Import::new(PathKind::from(expected_path))),
//         ))));
//         let generated_rust = generate(&mut target_ast, false, None).unwrap();
//         let target_rust = "use rula_lib as rula_std ; # [allow (unused)] mod rula { use super :: * ; use hello :: world ; } pub fn main () { }";
//         assert_eq!(generated_rust.to_string(), target_rust);
//         #[cfg(not(feature = "ci"))]
//         generate_token_stream_file(generated_rust, test_name!())
//     }

//     #[test]
//     #[ignore = "temporary"]
//     fn test_multi_import() {
//         // import hello::{world, there};
//         let expected_path_hello_world = ["hello", "world"].iter().collect();
//         let expected_path_hello_there = ["hello", "there"].iter().collect();

//         let expected_paths = vec![expected_path_hello_world, expected_path_hello_there];
//         let mut target_ast = build_stmt_ast(Stmt::new(StmtKind::Expr(Expr::new(
//             ExprKind::Import(Import::new(PathKind::from(expected_paths))),
//         ))));
//         let generated_rust = generate(&mut target_ast, false, None).unwrap();
//         let target_rust =
//             "mod rula { use hello :: world ; use hello :: there ; } pub fn main () { }";
//         assert_eq!(generated_rust.to_string(), target_rust);
//         #[cfg(not(feature = "ci"))]
//         generate_token_stream_file(generated_rust, test_name!())
//     }
// }

// mod if_ast_to_rust_tests {
//     use super::*;

//     #[test]
//     #[ignore = "temporary"]
//     fn test_simple_if_stmt() {
//         // if(block){expression}"
//         let mut target_ast =
//             build_stmt_ast(Stmt::new(StmtKind::Expr(Expr::new(ExprKind::If(If::new(
//                 // (block)
//                 Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
//                     "block",
//                     None,
//                     IdentType::Other,
//                 ))))),
//                 // {expression}
//                 vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
//                     Lit::new(LitKind::Ident(Ident::new(
//                         "expression",
//                         None,
//                         IdentType::Other,
//                     ))),
//                 ))))],
//                 // elif ~
//                 vec![],
//                 // else ~
//                 None,
//             ))))));
//         let generated_rust = generate(&mut target_ast, false, None).unwrap();
//         let target_rust = "mod rula { if block { expression } } pub fn main () { }";
//         assert_eq!(generated_rust.to_string(), target_rust);
//         // Printing out this make an error
//         // #[cfg(not(feature = "ci"))]
//         // generate_token_stream_file(generated_rust, test_name!())
//     }
// }

// mod interface_to_rust_tests {
//     use super::*;
//     #[test]
//     fn test_simple_interface_expr() {
//         let mut interface_ast = AstNode::RuLa(RuLa::new(RuLaKind::Program(Program::new(vec![
//             ProgramKind::Stmt(Stmt::new(StmtKind::Interface(Interface::new(
//                 vec![Ident::new("qn0", None, IdentType::Other)],
//                 Some(Ident::new("INTERFACE", None, IdentType::Other)),
//             )))),
//         ]))));

//         let generated_rust = generate(&mut interface_ast, false, None).unwrap();
//         let target_rust = get_correct_file_tokens("interface_def.rs");
//         // assert_eq!(generated_rust.to_string(), target_rust);
//         #[cfg(not(feature = "ci"))]
//         generate_token_stream_file(generated_rust, test_name!())
//     }
// }
