use std::borrow::BorrowMut;

use super::*;
// use crate::qubit::QubitInterface;
use rula_exec::ruleset_gen::{
    action::*,
    types::{Qubit, RuLaResult, RuleVec},
};

pub fn bsm(rules: RuleVec, q1: &Qubit, q2: &Qubit) -> RuLaResult {
    let q1_identifier = QubitIdentifier {
        qubit_index: q1.index,
    };
    let q2_identifier = QubitIdentifier {
        qubit_index: q2.index,
    };

    // 1. Apply cx betwen two
    let mut circuit = QCirc::new();
    circuit.add_qgate(QGate::new(q1_identifier.clone(), QGateType::CxControl));
    circuit.add_qgate(QGate::new(q2_identifier.clone(), QGateType::CxTarget));
    for rule in rules.borrow().iter() {
        rule.borrow_mut()
            .add_action_clause(ActionClauses::QCirc(circuit.clone()));
    }
    // 2. measure x for q1 and z for q2
    let meas_x = ActionClauses::Measure(Measure::new(q1_identifier.clone(), MeasBasis::X));
    let meas_z = ActionClauses::Measure(Measure::new(q2_identifier.clone(), MeasBasis::Z));
    for rule in rules.borrow().iter() {
        rule.borrow_mut().add_action_clause(meas_x.clone());
        rule.borrow_mut().add_action_clause(meas_z.clone());
    }
    let result = RuLaResult::new();
    result
}

pub fn x(rules: RuleVec) {}

pub fn z(rules: RuleVec) {}

pub fn cx(rules: RuleVec, q1: &Qubit, q2: &Qubit) {
    let q1_identifier = QubitIdentifier {
        qubit_index: q1.index,
    };
    let q2_identifier = QubitIdentifier {
        qubit_index: q2.index,
    };

    // apply cx
    let mut circuit = QCirc::new();
    circuit.add_qgate(QGate::new(q1_identifier.clone(), QGateType::CxControl));
    circuit.add_qgate(QGate::new(q2_identifier.clone(), QGateType::CxTarget));
    for rule in rules.borrow().iter() {
        rule.borrow_mut()
            .add_action_clause(ActionClauses::QCirc(circuit.clone()));
    }
}

pub fn measure(rules: RuleVec, q1: &Qubit, basis: &str) -> RuLaResult {
    match basis {
        "X" => meas_x(Rc::clone(&rules), q1),
        "Y" => meas_y(Rc::clone(&rules), q1),
        "Z" => meas_z(Rc::clone(&rules), q1),
        _ => {
            todo!("Unknown measurement basis {}", basis)
        }
    }
    RuLaResult::new()
}

fn meas_x(rules: RuleVec, q1: &Qubit) {
    for rule in rules.borrow().iter() {
        let q1_identifier = QubitIdentifier {
            qubit_index: q1.index,
        };
        rule.borrow_mut()
            .add_action_clause(ActionClauses::Measure(Measure::new(
                q1_identifier,
                MeasBasis::X,
            )))
    }
}

fn meas_y(rules: RuleVec, q1: &Qubit) {
    for rule in rules.borrow().iter() {
        let q1_identifier = QubitIdentifier {
            qubit_index: q1.index,
        };
        rule.borrow_mut()
            .add_action_clause(ActionClauses::Measure(Measure::new(
                q1_identifier,
                MeasBasis::Y,
            )))
    }
}

fn meas_z(rules: RuleVec, q1: &Qubit) {
    for rule in rules.borrow().iter() {
        let q1_identifier = QubitIdentifier {
            qubit_index: q1.index,
        };
        rule.borrow_mut()
            .add_action_clause(ActionClauses::Measure(Measure::new(
                q1_identifier,
                MeasBasis::Z,
            )))
    }
}
