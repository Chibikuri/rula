use super::ruleset::PartnerAddr;
use core::time::Duration;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Action {
    pub name: Option<String>,
    pub clauses: Vec<ActionClauses>,
}

impl Action {
    pub fn new(action_name: Option<String>) -> Self {
        Action {
            name: action_name,
            clauses: vec![],
        }
    }

    pub fn update_action_name(&mut self, action_name: Option<String>) {
        self.name = action_name;
    }

    pub fn add_action_clause(&mut self, action_clause: ActionClauses) {
        self.clauses.push(action_clause);
    }
}

// TODO: the field name might be different https://serde.rs/field-attrs.html
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum ActionClauses {
    // A series of unitary gate operations
    QCirc(QCirc),
    /// Measurement operations that takes calssical information from qubits
    Measure(Measure),
    /// Set a variable in Rule and RuleSet
    Set(Set),
    /// Free consumed resource for later use
    Free(QubitIdentifier),
    /// Promote resource from one rule to the next rule
    Promote(QubitIdentifier),
    /// Set the timer of
    SetTimer(SetTimer),
    /// Send classical message from one place to another
    Send(ProtocolMessages),
}

// In the future, to identify
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct QubitIdentifier {
    pub qubit_index: u32,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct QCirc {
    qgates: Vec<QGate>,
}

impl QCirc {
    pub fn new() -> Self {
        QCirc { qgates: vec![] }
    }

    pub fn from(qcircuit: Vec<QGate>) -> Self {
        QCirc { qgates: qcircuit }
    }

    pub fn add_qgate(&mut self, gate: QGate) {
        self.qgates.push(gate);
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct QGate {
    pub qubit_identifier: QubitIdentifier,
    pub kind: QGateType,
}

impl QGate {
    pub fn new(index: QubitIdentifier, gate_kind: QGateType) -> Self {
        QGate {
            qubit_identifier: index,
            kind: gate_kind,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum QGateType {
    X,
    Y,
    Z,
    H,
    CxControl,
    CxTarget,
    CzControl,
    CzTarget,
    Rx(f64),
    Ry(f64),
    Rz(f64),
    U(f64, f64, f64),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Measure {
    pub qubit_identifier: QubitIdentifier,
    pub basis: MeasBasis,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum MeasBasis {
    X,
    Y,
    Z,
    U(f64, f64, f64),
}

impl Measure {
    pub fn new(index: QubitIdentifier, basis: MeasBasis) -> Self {
        Measure {
            qubit_identifier: index,
            basis: basis,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Set {
    pub variable_type: VariableType,
}

// Variable Type takes new type
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum VariableType {
    MeasurementCount(u32),
    Fidelity(f64),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Free {
    pub qubit_index: u32,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct SetTimer {
    pub timer_id: TimerId,
}

// In the future, this timer should be classified to several candidates
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum TimerId {
    General(Duration),
}

// At the level of RuleSet, the outcome has not yet
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum ProtocolMessages {
    Free(ProtoMessageIdentifier),
    Update(ProtoMessageIdentifier),
    Meas(ProtoMessageIdentifier),
    Transfer(ProtoMessageIdentifier),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct ProtoMessageIdentifier {
    pub partner_addr: PartnerAddr,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_action_clause() {
        let mut action = Action::new(None);
        let mut qcirc = QCirc::new();
        let qgate = QGate::new(QubitIdentifier { qubit_index: 0 }, QGateType::H);
        qcirc.add_qgate(qgate);
        let clause = ActionClauses::QCirc(qcirc.clone());
        action.add_action_clause(clause);
        assert_eq!(action.name, None);
        assert_eq!(action.clauses.len(), 1);
        assert_eq!(action.clauses[0], ActionClauses::QCirc(qcirc));
    }

    #[test]
    fn test_measure_clause() {
        let measure_clause = Measure::new(QubitIdentifier { qubit_index: 0 }, MeasBasis::X);
        assert_eq!(measure_clause.basis, MeasBasis::X);
    }
}
