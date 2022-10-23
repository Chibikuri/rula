use rula_exec::rulep::action::v2::ActionClauses;
use rula_exec::rulep::ruleset::RuleSet;

use std::fs::File;
use std::io::Read;

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
        let ast = rula_parser::parse(&contents).unwrap();
        // println!("{:#?}", &ast);

        // 2. generate ruleset (provide ruleset flag)
        let (_, ruleset) = rula_exec::codegen::generator::generate(ast, true).unwrap();
        let target_ruleset = RuleSet::<ActionClauses>::new("entanglement_swapping");
        match ruleset {
            Some(ruleset_contents) => {
                assert_eq!(target_ruleset, ruleset_contents)
            }
            None => panic!("No ruleset found in the test"),
        }
    }
}

mod purification_ruleset {}

mod error_correction_ruleset {}

mod combination_ruleset {}
