use proc_macro2::TokenStream;
use std::env;
use std::fs::File;
use std::io::{Read, Write};
use std::process::Command;

// Helper function to generate file
pub fn generate_token_stream_file(program: TokenStream, file_name: &str) {
    let mut file_path = env::current_dir().unwrap();
    file_path.push("tests");
    file_path.push(file_name);
    let mut file = File::create(file_path.clone()).unwrap();
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

// Generate swapping rule executable
#[cfg(test)]
mod generate_swapping_rust {
    use super::*;

    #[test]
    fn gen_swapping() {
        // 0. load example file
        let mut rula_program =
            File::open("../examples/entanglement_swapping.rula").expect("target file not found");
        let mut contents = String::new();
        rula_program
            .read_to_string(&mut contents)
            .expect("Something went wrong reading the file");
        // 1. parse and generate ast
        let mut ast = rula_parser::parse(&contents).unwrap();
        // println!("{:#?}", &ast);

        // 2. generate ruleset (provide ruleset flag)
        let generated = rula_exec::codegen::generator::generate(&mut ast, true).unwrap();
        generate_token_stream_file(generated, "test2.rs");
        assert_eq!(1, 2);
    }
}
