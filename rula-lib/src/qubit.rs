use super::*;
use crate::ruleset::action::v2::*;
use serde::{Deserialize, Serialize};
use std::{cell::Cell, net::IpAddr};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct QubitInterface {
    pub busy: Cell<bool>,
    pub parent_qnic_address: Option<IpAddr>,
    pub expected_fidelity: f64,
}
impl QubitInterface {
    pub fn new() -> Self {
        QubitInterface {
            busy: Cell::new(false),
            parent_qnic_address: None,
            expected_fidelity: 0.0,
        }
    }
    pub async fn ready(&self) -> bool {
        true
    }
    pub fn __static__ready(&self, _: RuleVec) -> bool {
        true
    }
    pub async fn x(&self) {}

    pub fn __static__x(&self, rules: RuleVec) {
        for rule in &*rules.borrow_mut() {
            rule.borrow_mut()
                .add_action_clause(ActionClauses::Gate(QGate::new(QGateType::X, 0)));
        }
    }
    pub async fn z(&self) {}
    pub fn __static__z(&self, rules: RuleVec) {
        for rule in &*rules.borrow_mut() {
            rule.borrow_mut()
                .add_action_clause(ActionClauses::Gate(QGate::new(QGateType::Z, 0)));
        }
    }

    pub async fn cx(&self, _target: &QubitInterface) {}
    pub fn __static__cx(&self, rules: RuleVec, target: QubitInterface) {
        for rule in &*rules.borrow_mut() {
            // TODOShould we combine them?
            rule.borrow_mut()
                .add_action_clause(ActionClauses::Gate(QGate::new(QGateType::CxControl, 0)));
            rule.borrow_mut()
                .add_action_clause(ActionClauses::Gate(QGate::new(QGateType::CxTarget, 1)));
        }
    }

    pub async fn measure_z(&self) -> String {
        String::from("measure z")
    }
    pub fn __static__measure_z(&self, rules: RuleVec) -> String {
        for rule in &*rules.borrow_mut() {
            rule.borrow_mut()
                .add_action_clause(ActionClauses::Measure(Measure::new(MeasBasis::Z, 0)));
        }
        String::from("static_measure_z")
    }

    pub async fn expected_fidelity(&self) -> f64 {
        self.expected_fidelity
    }
    pub fn __static__expected_fidelity(&self, _: RuleVec) -> f64 {
        0.8
    }

    pub fn is_busy(&self) -> bool {
        self.busy.get()
    }
    pub fn set_busy(&self) {
        self.busy.set(true);
    }
    pub fn gen_mock() -> Self {
        QubitInterface {
            busy: Cell::new(false),
            parent_qnic_address: None,
            expected_fidelity: 0.8,
        }
    }
}
