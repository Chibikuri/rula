use proc_macro2::TokenStream;
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::process::Command;
use std::{env, fs};

use clap::Parser;

// enum OutputFormat{
//     Json,
//     MsgPack
// }
/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(name="rula", author, version, about, long_about = None)]
struct Args {
    /// Rula program path
    rula_program: PathBuf,

    /// RuLa configuration
    config: PathBuf,

    /// Output directory path
    output_dir_path: Option<PathBuf>,

    /// Output serialization format (e.g. JSON)
    serialization_format: Option<String>,
    /// If this is true, leave the rust code that generates RuleSet json
    #[arg(short, long, default_value_t = false)]
    debug: bool,
}

fn generate_token_stream_file(program: TokenStream, file_path: &PathBuf) {
    let mut f = File::create(file_path).unwrap();
    f.write_all(program.to_string().as_bytes())
        .expect("Unable to write data");
    // Format generated file
    #[cfg(not(feature = "no-format"))]
    Command::new("cargo")
        .arg("fmt")
        .arg("--")
        .arg(file_path)
        .spawn()
        .expect("Command failed");
}

fn main() {
    let args = Args::parse();
    let mut rula_program = File::open(&args.rula_program).expect("No such file");
    let mut contents = String::new();
    rula_program
        .read_to_string(&mut contents)
        .expect("Something went wrong reading the file");
    // Path to the config
    let config = args.config;
    // parse rula program
    let ast = rula_parser::parse(&contents, &args.rula_program).unwrap();
    let program = rula_exec::ruleset_gen::ruleset_generator::generate(&ast, config).unwrap();

    // Generate RuleSet program
    let mut file_path = env::current_dir().unwrap();
    file_path.push(".rula_generated.rs");
    generate_token_stream_file(program, &file_path);

    // Execute RuleSet generator
    // Command::new("cargo").arg("run").arg()
    Command::new("echo")
        .arg("Hello")
        .spawn()
        .expect("Command Failed");

    // Command::new("cargo")
    // Command::new("rm")
    //     .arg(".rula_generated.rs")
    //     .spawn()
    //     .expect("Failed to remove the generated file");
}
