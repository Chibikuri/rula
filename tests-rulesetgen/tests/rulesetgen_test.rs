use rula_exec::codegen::generator::*;
use rula_parser::parser::ast::*;

use std::fs::File;
use std::io::Read;
use std::process::Command;

// E2E test for generating swapping RuleSet
#[cfg(test)]
mod generate_swapping_ruleset {
    use super::*;
    // target: examples/entanglment_swapping.rula
    #[test]
    fn test_swapping_rule_gen() {
        // 0. load example file
        let mut rula_program =
            File::open("../examples/entanglement_swapping.rula").expect("target file not found");
        let mut contents = String::new();
        rula_program
            .read_to_string(&mut contents)
            .expect("Something went wrong reading the file");
        // 1. parse and generate ast
        // let ast = rula_parser::parse(&contents).unwrap();
        // let (_, ruleset) = rula_exec::codegen::generator::generate(ast, true).unwrap();

        // 2. generate ruleset (provide ruleset flag)
    }
}

mod purification_ruleset {}

mod error_correction_ruleset {}

mod combination_ruleset {}
