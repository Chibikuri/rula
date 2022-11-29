use super::*;
use crate::qubit::QubitInterface;
use crate::ruleset::action::v2::*;

pub async fn bsm(q1: &QubitInterface, q2: &QubitInterface) -> String {
    String::from("result")
}
pub fn __static__bsm(rules: RuleVec, q1: QubitInterface, q2: QubitInterface) -> String {
    for rule in &*rules.borrow_mut() {
        rule.borrow_mut()
            .add_action_clause(ActionClauses::Gate(QGate::new(QGateType::CxControl, 0)));
        rule.borrow_mut()
            .add_action_clause(ActionClauses::Gate(QGate::new(QGateType::CxTarget, 1)));
        rule.borrow_mut()
            .add_action_clause(ActionClauses::Measure(Measure::new(MeasBasis::X, 0)));
        rule.borrow_mut()
            .add_action_clause(ActionClauses::Measure(Measure::new(MeasBasis::Z, 1)))
    }
    String::from("static __result")
}
