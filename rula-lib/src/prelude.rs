use rula_exec::ruleset_gen::action::*;
use rula_exec::ruleset_gen::condition::*;
use rula_exec::ruleset_gen::ruleset::*;
use rula_exec::ruleset_gen::types::*;
use std::cell::RefCell;
use std::collections::HashSet;

pub fn res(
    rules: RuleVec,
    num_res: u64,
    req_fidelity: f64,
    partner_repeater: &Repeater,
    qubit_index: u64,
) -> Qubit {
    for rule in rules.borrow().iter() {
        rule.borrow_mut()
            .add_condition_clause(ConditionClauses::Res(Res::new(
                num_res,
                PartnerAddr::IntegerKind(partner_repeater.index),
                Some(req_fidelity),
            )))
    }
    Qubit::new(qubit_index)
}

pub fn check_timer(rules: RuleVec, timer_id: String) {
    for rule in rules.borrow().iter() {
        rule.borrow_mut()
            .add_condition_clause(ConditionClauses::Timer(10.0));
    }
    todo!("Need fix");
}

pub fn cmp(rules: RuleVec) {
    todo!("Add!")
}

pub fn set_timer(rules: RuleVec) {
    todo!("Add timer");
}

pub fn send(rules: RuleVec, partner_repeater: &Repeater, proto_message_type: ProtoMessageType) {
    // 1. Create send instruction
    let proto_message_identifier = ProtoMessageIdentifier {
        partner_addr: PartnerAddr::IntegerKind(partner_repeater.index),
    };
    let protocol_message = generate_protocol_message(proto_message_type, proto_message_identifier);
    for rule in rules.borrow().iter() {
        rule.borrow_mut()
            .add_action_clause(ActionClauses::Send(protocol_message.clone()));
    }
}

pub fn wait(
    partner_rules: RuleVec,
    swapping_repeater: &Repeater,
    proto_message_type: ProtoMessageType,
) {
    let identifier = ProtoMessageIdentifier::new(AddressKind::IntegerKind(swapping_repeater.index));
    let proto_message = generate_protocol_message(proto_message_type, identifier);
    let condition_clause = ConditionClauses::Wait(proto_message.clone());
    let action_clause = ActionClauses::Wait(proto_message.clone());
    for rule in partner_rules.borrow().iter() {
        rule.borrow_mut()
            .add_condition_clause(condition_clause.clone());
        rule.borrow_mut().add_action_clause(action_clause.clone());
    }
}

pub fn recv(rules: RuleVec, source_repeater: &Repeater) {}

fn generate_protocol_message(
    proto_message_type: ProtoMessageType,
    identifier: ProtoMessageIdentifier,
) -> ProtocolMessages {
    match proto_message_type {
        ProtoMessageType::Free => ProtocolMessages::Free(identifier),
        ProtoMessageType::Update => ProtocolMessages::Update(identifier),
        ProtoMessageType::Meas => ProtocolMessages::Meas(identifier),
        ProtoMessageType::Transfer => ProtocolMessages::Transfer(identifier),
    }
}
