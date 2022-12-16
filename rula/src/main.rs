use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use clap::Parser;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(name="rula", author, version, about, long_about = None)]
struct Args {
    // rula program name
    rula_program: PathBuf,

    config: Option<PathBuf>,
    /// Name of the person to greet
    #[arg(short, long, default_value = "false")]
    ruleset: bool,

    /// In the case where the condition never satisfy,
    /// if this value is false, that rule is not included in RuleSet
    #[arg(short, long, default_value = "false")]
    never_satisfy: bool,
}

// #[derive(Debug, Der)]
// struct Config{...}

fn main() {
    let args = Args::parse();
    let mut rula_program = File::open(args.rula_program).expect("No such file");
    let mut contents = String::new();
    rula_program
        .read_to_string(&mut contents)
        .expect("Something went wrong reading the file");

    let config = match args.config {
        Some(conf) => Some(conf),
        None => None,
    };
    println!("config{:#?}", config);
    // parse rula program
    let mut ast = rula_parser::parse(&contents).unwrap();
    // let _generated =
    //     rula_exec::codegen::generator::generate(&mut ast, args.ruleset, config).unwrap();
    if args.ruleset {
        println!("Generating RuleSet...");
    }
    println!("Hello {}!", args.ruleset);
}
