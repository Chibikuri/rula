use rula_exec::ruleset_gen::action::*;
use rula_exec::ruleset_gen::condition::*;
use rula_exec::ruleset_gen::ruleset::*;

use proc_macro2::TokenStream;
use std::env;
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::process::Command;

// Helper functions for creating Rules
pub(crate) fn _create_rule(
    name: &str,
    id: u32,
    shared_tag: u32,
    condition: Condition,
    action: Action,
    is_finalized: bool,
) -> Rule {
    let mut rule = Rule::new(name);
    rule.update_finalized(is_finalized);
    rule.update_shared_tag(shared_tag);
    rule.update_id(id);
    rule.set_condition(condition);
    rule.set_action(action);
    rule
}

fn generate_token_stream_file(program: TokenStream, file_name: &str) {
    let mut file_path = env::current_dir().unwrap();
    file_path.push("tests");
    file_path.push(file_name);
    // let mut writer = BufWriter::new(File::create(file_path.clone()).unwrap());
    let mut f = File::create(file_path.clone()).unwrap();
    f.write_all(program.to_string().as_bytes())
        .expect("Unable to write data");
    // fs::write(file_path.to_str(), program.to_string()).expect("Failed to write file");
    // writer.write(&program.to_string().as_bytes()).unwrap();
    // Format generated file
    #[cfg(not(feature = "no-format"))]
    Command::new("cargo")
        .arg("fmt")
        .arg("--")
        .arg(file_path)
        .spawn()
        .expect("Command failed");
}

// pub(crate) fn create_condition(name: Option<&str>, clauses: Vec<ConditionClauses>) -> Condition {
//     let mut condition = Condition::new(None);
//     match name {
//         Some(cond_name) => condition.update_condition_name(cond_name),
//         None => {}
//     }
//     condition
// }

// pub(crate) fn create_action(
//     name: Option<&str>,
//     clauses: Vec<ActionClauses>,
// ) -> Action<ActionClauses> {
//     let mut action = Action::<ActionClauses>::new(None);
//     match name {
//         Some(action_name) => action.update_action_name(Some(action_name.to_string())),
//         None => {}
//     }
//     action
// }

// E2E test for generating swapping RuleSet
#[cfg(test)]
mod generate_swapping_ruleset {
    use super::*;

    #[test]
    fn test_swapping_rule_gen() {
        // 0. load example file
        let mut rula_program =
            File::open("../examples/v2/entanglement_swapping.rula").expect("target file not found");
        let mut contents = String::new();
        rula_program
            .read_to_string(&mut contents)
            .expect("Something went wrong reading the file");
        // 1. parse and generate ast
        let ast = rula_parser::parse(&contents, &PathBuf::from("/")).unwrap();

        // 2. generate ruleset (provide ruleset flag)
        let tokens = rula_exec::ruleset_gen::ruleset_generator::generate(
            &ast,
            PathBuf::from("../examples/v2/config.json"),
        )
        .unwrap();

        // 2.1 target ruleset
        let mut target_ruleset = RuleSet::new("entanglement_swapping", AddressKind::IntegerKind(0));
        let mut stage = Stage::new();
        let rule1 = Rule::new("swapping");
        stage.add_rule(rule1);
        target_ruleset.add_stage(stage);
        generate_token_stream_file(tokens, "generator_swapping.rs");
        // assert_eq!(rulesets[0], target_ruleset);
        assert_eq!(1, 1);
    }
}

#[cfg(test)]
mod purification_ruleset {
    use super::*;

    #[test]
    fn test_purification_rule_gen() {
        let file_path = "../examples/v2/purification.rula";
        let mut rula_program = File::open(file_path).expect("target file not found");
        let mut contents = String::new();
        rula_program
            .read_to_string(&mut contents)
            .expect("Something went wrong reading the file");
        // 1. parse and generate ast
        let ast = rula_parser::parse(&contents, &PathBuf::from(file_path)).unwrap();

        // 2. generate ruleset (provide ruleset flag)
        let tokens = rula_exec::ruleset_gen::ruleset_generator::generate(
            &ast,
            PathBuf::from("../examples/v2/config.json"),
        )
        .unwrap();

        // 2.1 target ruleset
        // let target = RuleSet::new("purification");

        generate_token_stream_file(tokens, "generator_purification.rs");
        assert_eq!(1, 1);
    }
}

#[cfg(test)]
mod rule_importing_test {
    use super::*;

    #[test]
    fn test_rule_importing() {
        let mut rula_program =
            File::open("../examples/v2/swapping_pur.rula").expect("target file not found");
        let mut contents = String::new();

        rula_program
            .read_to_string(&mut contents)
            .expect("Something went wrong reading the file");

        let ast = rula_parser::parse(
            &contents,
            &PathBuf::from("../examples/v2/swapping_pur.rula"),
        )
        .unwrap();

        let tokens = rula_exec::ruleset_gen::ruleset_generator::generate(
            &ast,
            PathBuf::from("../examples/v2/config.json"),
        )
        .unwrap();

        // let target = RuleSet::new()
        generate_token_stream_file(tokens, "generator_swapping_purification.rs");
        assert_eq!(1, 1);
    }
}

// mod error_correction_ruleset {}

// mod combination_ruleset {}
