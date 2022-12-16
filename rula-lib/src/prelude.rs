use rula_exec::ruleset_gen::action::ActionClauses;
use rula_exec::ruleset_gen::action::ProtoMessageIdentifier;
use rula_exec::ruleset_gen::action::ProtoMessageType;
use rula_exec::ruleset_gen::action::ProtocolMessages;
use rula_exec::ruleset_gen::condition::*;
use rula_exec::ruleset_gen::ruleset::*;
use rula_exec::ruleset_gen::types::*;

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
                PartnerAddr::IntegerKind(partner_repeater.address),
                Some(req_fidelity),
            )))
    }
    Qubit::new(qubit_index)
}

pub fn send(rules: RuleVec, partner_repeater: &Repeater, proto_message_type: ProtoMessageType) {
    let proto_message_identifier = ProtoMessageIdentifier {
        partner_addr: PartnerAddr::IntegerKind(partner_repeater.address),
    };
    let protocol_message = match proto_message_type {
        ProtoMessageType::Free => ProtocolMessages::Free(proto_message_identifier),
        ProtoMessageType::Update => ProtocolMessages::Update(proto_message_identifier),
        ProtoMessageType::Meas => ProtocolMessages::Meas(proto_message_identifier),
        ProtoMessageType::Transfer => ProtocolMessages::Transfer(proto_message_identifier),
    };
    for rule in rules.borrow().iter() {
        rule.borrow_mut()
            .add_action_clause(ActionClauses::Send(protocol_message.clone()));
    }
}
