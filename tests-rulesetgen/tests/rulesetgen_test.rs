use rula_exec::rulep::action::v2::*;
use rula_exec::rulep::action::Action;
use rula_exec::rulep::condition::v1::*;
use rula_exec::rulep::condition::Condition;
use rula_exec::rulep::ruleset::{Rule, RuleSet};

// Interface wrapper
use rula_exec::network::qnic_wrapper::Interface;
use rula_exec::network::qubit_wrapper::Qubit;

// Is this proper one?
use mock_components::hardware::qnic::QnicType;

use std::fs::File;
use std::io::Read;
use std::net::IpAddr;

// Helper functions for creating Rules
pub(crate) fn create_rule(
    name: &str,
    id: u32,
    shared_tag: u32,
    condition: Condition,
    action: Action<ActionClauses>,
    next_rule_id: u32,
    is_finalized: bool,
) -> Rule<ActionClauses> {
    let mut rule = Rule::<ActionClauses>::new(name);
    rule.update_finalized(is_finalized);
    rule.update_next_rule_id(next_rule_id);
    rule.update_shared_tag(shared_tag);
    rule.update_id(id);
    rule.set_condition(condition);
    rule.set_action(action);
    rule
}

pub(crate) fn create_condition(name: Option<&str>, clauses: Vec<ConditionClauses>) -> Condition {
    let mut condition = Condition::new(None);
    match name {
        Some(cond_name) => condition.update_condition_name(cond_name),
        None => {}
    }
    condition
}

pub(crate) fn create_action(
    name: Option<&str>,
    clauses: Vec<ActionClauses>,
) -> Action<ActionClauses> {
    let mut action = Action::<ActionClauses>::new(None);
    match name {
        Some(action_name) => action.update_action_name(Some(action_name.to_string())),
        None => {}
    }
    action
}

// E2E test for generating swapping RuleSet
#[cfg(test)]
mod generate_swapping_ruleset {
    use std::net::Ipv4Addr;

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

        // 2.1 target ruleset
        let mut target_ruleset = RuleSet::<ActionClauses>::new("entanglement_swapping");

        // 2.1.0
        // Entanglement Swapping Condition
        let enough_resource_clause_qn0 = ConditionClauses::EnoughResource(EnoughResource::new(
            1,
            0.8,
            Some(Interface::from(
                QnicType::QnicE,
                0,
                IpAddr::V4(Ipv4Addr::new(192, 168, 0, 1)),
            )),
        ));
        let enough_resource_clause_qn1 = ConditionClauses::EnoughResource(EnoughResource::new(
            1,
            0.8,
            Some(Interface::from(
                QnicType::QnicE,
                0,
                IpAddr::V4(Ipv4Addr::new(192, 168, 0, 3)),
            )),
        ));
        let mut condition = Condition::new(None);
        condition.add_condition_clause(enough_resource_clause_qn0);
        condition.add_condition_clause(enough_resource_clause_qn1);

        // Entanglement Swapping Action
        let cnot_control_gate = ActionClauses::Gate(QGate::new(QGateType::CxControl, Qubit::new()));
        let cnot_target_gate = ActionClauses::Gate(QGate::new(QGateType::CxControl, Qubit::new()));
        let send_result_qn0 = ActionClauses::Send(Send::new(
            IpAddr::V4(Ipv4Addr::new(192, 168, 0, 2)),
            IpAddr::V4(Ipv4Addr::new(192, 168, 0, 1)),
        ));
        let send_result_qn1 = ActionClauses::Send(Send::new(
            IpAddr::V4(Ipv4Addr::new(192, 168, 0, 2)),
            IpAddr::V4(Ipv4Addr::new(192, 168, 0, 3)),
        ));
        let mut action = Action::new(None);
        action.add_action_clause(cnot_control_gate);
        action.add_action_clause(send_result_qn0);
        action.add_action_clause(cnot_target_gate);
        action.add_action_clause(send_result_qn1);

        let rule = create_rule("swapping", 0, 0, condition, action, 0, true);
        target_ruleset.add_rule(rule);

        match ruleset {
            Some(ruleset_contents) => {
                println!("left: {:#?}", target_ruleset);
                println!("right: {:#?}", ruleset_contents);
                assert_eq!(target_ruleset, ruleset_contents)
            }
            None => panic!("No ruleset found in the test"),
        }
    }
}

mod purification_ruleset {}

mod error_correction_ruleset {}

mod combination_ruleset {}
