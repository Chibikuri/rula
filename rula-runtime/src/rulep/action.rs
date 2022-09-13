use crate::network::qubit::Qubit;
use serde::{Deserialize, Serialize};
use std::net::{IpAddr, Ipv4Addr};

pub mod v1 {
    use super::*;

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Action {
        pub name: Option<String>,
        pub clauses: Vec<ActionClausesV1>,
    }

    impl Action {
        pub fn new(action_name: Option<String>) -> Self {
            Action {
                name: action_name,
                clauses: vec![],
            }
        }
        pub fn add_action_clause(&mut self, action_clause: ActionClausesV1) {
            self.clauses.push(action_clause);
        }
    }
    // old version of action clauses
    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub enum ActionClausesV1 {
        Purification(Purification),
        EntanglementSwapping(EntanglementSwapping),
        Wait,
        Tomography(Tomography),
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Purification();
    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct EntanglementSwapping();
    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Tomography();
}

pub mod v2 {

    use super::*;

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
        pub fn add_action_clause(&mut self, action_clause: ActionClauses) {
            self.clauses.push(action_clause);
        }
    }
    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub enum ActionClauses {
        /// Gate operations that can be applied immediately
        Gate(QGate),
        /// Measurement operations that takes calssical information from qubits
        Measure(MeasBasis),
        /// Send classical message from one place to another
        Send(Message),
        /// Update the status of qubit
        Update(Qubit),
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct QGate {
        pub kind: GateType,
        pub target: Qubit,
    }

    impl QGate {
        pub fn new(gate_kind: GateType, target_qubit: Qubit) -> Self {
            QGate {
                kind: gate_kind,
                target: target_qubit,
            }
        }
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub enum GateType {
        X,
        Y,
        Z,
        H,
        Cx,
        Cz,
        Rx(f64),
        Ry(f64),
        Rz(f64),
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
        pub kind: String,
        pub src: IpAddr,
        pub dst: IpAddr,
        pub res: Box<Option<String>>,
    }
}
