use rula_parser::parser::ast::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct RuleMeta {
    pub watched_values: HashMap<String, Watchable>,
}

impl RuleMeta {
    pub fn new(watched_values: HashMap<String, Watchable>) -> Self {
        RuleMeta {
            watched_values: watched_values,
        }
    }
    pub fn place_holder() -> Self {
        RuleMeta {
            watched_values: HashMap::new(),
        }
    }
    pub fn insert_watch_value(&mut self, name: String, watchable: Watchable) {
        self.watched_values.insert(name, watchable);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Watchable {
    Quantum(QuantumProp),
    Classical(ClassicalProp),
    UnSet,
}

#[derive(Debug, Clone, PartialEq)]
pub struct QuantumProp {
    state_type: StateType,
    // Partner address identifier will be replaced by actual address in configuration
    partner_addresses: Vec<String>,
    required_number: i32,
    required_fidelity: Option<f64>,
}

impl QuantumProp {
    pub fn new(
        state_type: StateType,
        partner_addrs: Vec<String>,
        required_number: i32,
        required_fidelity: Option<f64>,
    ) -> Self {
        QuantumProp {
            state_type: state_type,
            partner_addresses: partner_addrs,
            required_number: required_number,
            required_fidelity: required_fidelity,
        }
    }
    pub fn place_holder() -> Self {
        QuantumProp {
            state_type: StateType::UnSet,
            partner_addresses: vec![],
            required_number: -1,
            required_fidelity: None,
        }
    }
    pub fn add_state_type(&mut self, new_state_type: StateType) {
        self.state_type = new_state_type;
    }
    pub fn add_partner(&mut self, partner: &str) {
        self.partner_addresses.push(String::from(partner));
    }
    pub fn update_required_number(&mut self, req_number: i32) {
        self.required_number = req_number;
    }
    pub fn update_required_fidelity(&mut self, req_fidelity: f64) {
        self.required_fidelity = Some(req_fidelity);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StateType {
    HalfBell,
    Graph,
    UnSet,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassicalProp {
    // The source identifier to be replaced by actual address in configuration
    src: String,
    kind: MessageKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MessageKind {
    Swapping,
    Purification,
    ErrorCorrection,
}
