use crate::qnic::QnicInterface;
use crate::qubit::QubitInterface;
use serde::{Deserialize, Serialize};
use std::net::IpAddr;

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Action<T> {
    pub name: Option<String>,
    pub clauses: Vec<T>,
}

impl<T> Action<T> {
    pub fn new(action_name: Option<String>) -> Self {
        Action {
            name: action_name,
            clauses: vec![],
        }
    }

    pub fn update_action_name(&mut self, action_name: Option<String>) {
        self.name = action_name;
    }

    pub fn add_action_clause(&mut self, action_clause: T) {
        self.clauses.push(action_clause);
    }
}

// Version 2 (Base actions)

pub mod v2 {
    use super::*;

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub enum ActionClauses {
        /// Gate operations that can be applied immediately
        Gate(QGate),
        /// Measurement operations that takes calssical information from qubits
        Measure(Measure),
        /// Send classical message from one place to another
        Send(Send),
        /// Free consumed resource for later use
        Free(QubitInterface),
        /// Update the status of qubit
        Update(Update),
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct QGate {
        pub kind: QGateType,
        pub target: QubitInterface,
    }

    impl QGate {
        pub fn new(gate_kind: QGateType, target_qubit: QubitInterface) -> Self {
            QGate {
                kind: gate_kind,
                target: target_qubit,
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
        pub basis: MeasBasis,
        pub target: QubitInterface,
    }

    impl Measure {
        pub fn new(basis: MeasBasis, target: QubitInterface) -> Self {
            Measure {
                basis: basis,
                target: target,
            }
        }
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Send {
        pub src: IpAddr,
        pub dst: IpAddr,
    }

    impl Send {
        pub fn new(src: IpAddr, dst: IpAddr) -> Self {
            Send { src: src, dst: dst }
        }
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub enum MeasBasis {
        X,
        Y,
        Z,
        U(f64, f64, f64),
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Message {
        pub meta: MetaData,
        pub kind: MessageKind,
        pub result: MeasResult,
    }
    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct MetaData {
        pub src: IpAddr,
        pub dst: IpAddr,
        pub shared_id: u128,
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub enum MessageKind {
        /// Message for purification
        PurificationResult,
        SwappingResult,
        MeasureResult,
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct MeasResult {
        pub basis: MeasBasis,
        pub result: MeasOutput,
        pub interface_info: QnicInterface,
        pub qubit_info: QubitInterface,
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub enum MeasOutput {
        Zero,
        One,
    }
    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Update {
        pub target: QubitInterface,
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_action_clause() {
            let mut action = Action::new(None);
            let qgate = QGate::new(QGateType::H, QubitInterface::new());
            let clause = ActionClauses::Gate(qgate.clone());
            action.add_action_clause(clause);
            assert_eq!(action.name, None);
            assert_eq!(action.clauses.len(), 1);
            assert_eq!(action.clauses[0], ActionClauses::Gate(qgate));
        }

        #[test]
        fn test_measure_clause() {
            let measure_clause = Measure::new(MeasBasis::X, QubitInterface::new());
            assert_eq!(measure_clause.basis, MeasBasis::X);
            assert_eq!(measure_clause.target, QubitInterface::new())
        }
    }
}
