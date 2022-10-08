use clap::Parser;
use rula_exec;
use rula_parser::parse as rula_parse;
use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(name="rula", author, version, about, long_about = None)]
struct Args {
    // rula program name
    rula_program: PathBuf,

    /// Name of the person to greet
    #[arg(short, long, default_value = "false")]
    ruleset: bool,
    // Number of times to greet
    // #[arg(short, long, default_value_t = 1)]
    // count: u8,
}

fn main() {
    let args = Args::parse();
    let mut rula_program = File::open(args.rula_program).expect("No such file");
    let mut contents = String::new();
    rula_program
        .read_to_string(&mut contents)
        .expect("Something went wrong reading the file");
    // parse rula program
    let ast = rula_parser::parse(&contents).unwrap();
    let rust = rula_exec::codegen::generator::generate(ast).unwrap();
    println!("{:#?}", rust);
    // println!("{:#?}", ast);
    if !args.ruleset {
        println!("Generating RuleSet...");
    }
    println!("Hello {}!", args.ruleset);
}
