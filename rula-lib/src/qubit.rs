use super::*;
use crate::ruleset::action::v2::*;
use serde::{Deserialize, Serialize};
use std::{cell::Cell, net::IpAddr};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct QubitInterface {
    pub busy: Cell<bool>,
    pub parent_qnic_address: Option<IpAddr>,
}
impl QubitInterface {
    pub fn new() -> Self {
        QubitInterface {
            busy: Cell::new(false),
            parent_qnic_address: None,
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
        }
    }
}
