use rula_exec::ruleset_gen::action::*;
use rula_exec::ruleset_gen::condition::*;
use rula_exec::ruleset_gen::ruleset::*;

use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

// Helper functions for creating Rules
pub(crate) fn create_rule(
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
        let mut ast = rula_parser::parse(&contents).unwrap();

        // 2. generate ruleset (provide ruleset flag)
        let rulesets = rula_exec::ruleset_gen::ruleset_generator::generate(
            &ast,
            PathBuf::from("../examples/v2/config.json"),
        )
        .unwrap();

        // 2.1 target ruleset
        let mut target_ruleset = RuleSet::new("entanglement_swapping");
        let mut stage = Stage::new();
        let rule1 = Rule::new("swapping");
        stage.add_rule(rule1);
        target_ruleset.add_stage(stage);

        assert_eq!(
            rulesets.get(&0).expect("failed to get rulesets").clone(),
            target_ruleset
        );
        // 2.1.0
        // Entanglement Swapping Condition
        // let enough_resource_clause_qn0 = ConditionClauses::EnoughResource(EnoughResource::new(
        //     1,
        //     Some(0.8),
        //     Some(::new(QnicType::QnicE, 0, None)),
        // ));
        // let enough_resource_clause_qn1 = ConditionClauses::EnoughResource(EnoughResource::new(
        //     1,
        //     Some(0.8),
        //     Some(QnicInterfaceWrapper::new(QnicType::QnicE, 0, None)),
        // ));
        // let mut condition = Condition::new(None);
        // condition.add_condition_clause(enough_resource_clause_qn0);
        // condition.add_condition_clause(enough_resource_clause_qn1);

        // // Entanglement Swapping Action
        // let cnot_control_gate = ActionClauses::Gate(QGate::new(
        //     QGateType::CxControl,
        //     QubitInterfaceWrapper::new(),
        // ));
        // let cnot_target_gate = ActionClauses::Gate(QGate::new(
        //     QGateType::CxControl,
        //     QubitInterfaceWrapper::new(),
        // ));
        // let send_result_qn0 = ActionClauses::Send(Send::new(
        //     IpAddr::V4(Ipv4Addr::new(192, 168, 0, 2)),
        //     IpAddr::V4(Ipv4Addr::new(192, 168, 0, 1)),
        // ));
        // let send_result_qn1 = ActionClauses::Send(Send::new(
        //     IpAddr::V4(Ipv4Addr::new(192, 168, 0, 2)),
        //     IpAddr::V4(Ipv4Addr::new(192, 168, 0, 3)),
        // ));
        // let mut action = Action::new(None);
        // action.add_action_clause(cnot_control_gate);
        // action.add_action_clause(send_result_qn0);
        // action.add_action_clause(cnot_target_gate);
        // action.add_action_clause(send_result_qn1);

        // let rule = create_rule("swapping", 0, 0, condition, action, 0, true);
        // target_ruleset.add_rule(rule);

        // match ruleset {
        //     Some(ruleset_contents) => {
        //         println!("left: {:#?}", target_ruleset);
        //         println!("generated: {:#?}", ruleset_contents);
        //         assert_eq!(target_ruleset, ruleset_contents)
        //     }
        //     None => panic!("No ruleset found in the test"),
        // }
    }
}

#[cfg(test)]
mod purification_ruleset {
    use super::*;

    #[test]
    fn test_purification_rule_gen() {
        let mut rula_program =
            File::open("../examples/v2/purification.rula").expect("target file not found");
        let mut contents = String::new();
        rula_program
            .read_to_string(&mut contents)
            .expect("Something went wrong reading the file");
        // 1. parse and generate ast
        let mut ast = rula_parser::parse(&contents).unwrap();

        // 2. generate ruleset (provide ruleset flag)
        let rulesets = rula_exec::ruleset_gen::ruleset_generator::generate(
            &ast,
            PathBuf::from("../examples/v2/config.json"),
        )
        .unwrap();

        // 2.1 target ruleset
        let mut target_rulesets = vec![RuleSet::new("purification")];

        for (i, ruleset) in rulesets {
            println!("{:#?}", ruleset);
            assert_eq!(ruleset, target_rulesets[i as usize]);
        }
    }
}

// mod error_correction_ruleset {}

// mod combination_ruleset {}
