use super::ruleset::InterfaceInfo;
use serde::{Serialize, Deserialize};
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

    #[derive(Serialize, Debug, PartialEq, Clone)]
    pub enum ActionClauses {
        /// Gate operations that can be applied immediately
        Gate(QGate),
        /// Measurement operations that takes calssical information from qubits
        Measure(Measure),
        /// Send classical message from one place to another
        Send(Send),
        /// Free consumed resource for later use
        Free,
        /// Update the status of qubit
        Update(Update),
    }

    #[derive(Serialize, Debug, PartialEq, Clone)]
    pub struct QGate {
        pub kind: QGateType,
    }

    impl QGate {
        pub fn new(gate_kind: QGateType) -> Self {
            QGate { kind: gate_kind }
        }
    }

    #[derive(Serialize, Debug, PartialEq, Clone)]
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

    #[derive(Serialize, Debug, PartialEq, Clone)]
    pub struct Measure {
        pub basis: MeasBasis,
    }

    impl Measure {
        pub fn new(basis: MeasBasis) -> Self {
            Measure { basis: basis }
        }
    }

    #[derive(Serialize, Debug, PartialEq, Clone)]
    pub struct Send {
        pub src: IpAddr,
        pub dst: IpAddr,
    }

    impl Send {
        pub fn new(src: IpAddr, dst: IpAddr) -> Self {
            Send { src: src, dst: dst }
        }
    }

    #[derive(Serialize, Debug, PartialEq, Clone)]
    pub enum MeasBasis {
        X,
        Y,
        Z,
        U(f64, f64, f64),
    }

    #[derive(Serialize, Debug, PartialEq, Clone)]
    pub struct Message {
        pub meta: MetaData,
        pub kind: MessageKind,
        pub result: MeasResult,
    }
    #[derive(Serialize, Debug, PartialEq, Clone)]
    pub struct MetaData {
        pub src: IpAddr,
        pub dst: IpAddr,
        pub shared_id: u128,
    }

    #[derive(Serialize, Debug, PartialEq, Clone)]
    pub enum MessageKind {
        /// Message for purification
        PurificationResult,
        SwappingResult,
        MeasureResult,
    }

    #[derive(Serialize, Debug, PartialEq, Clone)]
    pub struct MeasResult {
        pub basis: MeasBasis,
        pub result: MeasOutput,
        pub interface_info: InterfaceInfo,
    }

    #[derive(Serialize, Debug, PartialEq, Clone)]
    pub enum MeasOutput {
        Zero,
        One,
    }
    #[derive(Serialize, Debug, PartialEq, Clone)]
    pub struct Update {}

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_action_clause() {
            let mut action = Action::new(None);
            let qgate = QGate::new(QGateType::H);
            let clause = ActionClauses::Gate(qgate.clone());
            action.add_action_clause(clause);
            assert_eq!(action.name, None);
            assert_eq!(action.clauses.len(), 1);
            assert_eq!(action.clauses[0], ActionClauses::Gate(qgate));
        }

        #[test]
        fn test_measure_clause() {
            let measure_clause = Measure::new(MeasBasis::X);
            assert_eq!(measure_clause.basis, MeasBasis::X);
        }
    }
}
