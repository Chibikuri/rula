use crate::network::qnic::Interface;
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
    pub struct Purification {
        pub purification_type: PurType,
        pub qnic_interface: Interface,
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub enum PurType {
        ///< Invalid purification type
        Invalid,
        ///< Single purification for X error
        SingleX,
        ///< Single purification for Z error
        SingleZ,
        ///< Double purification both for X and Z errors
        Double,
        ///< Double inverse purification both for X and Z errors
        DoubleInv,
        ///< Double selection XZ and single action (DoubleSelectionAction) for X error
        DsSa,
        ///< Inverse Double selection XZ and single action(DoubleSelectionAction) for X error
        DsSaInv,
        ///< Double Selection and Dual Action for both X and Z errors
        DsDa,
        ///< Inverse Double Selection and Dual Action for both X and Z errors
        DsDaInv,
        ///< Different type of Double Selection and Dual Action for both X and Z errors
        DsDaSecond,
        ///< Different type of Inverse Double Selection and Dual Action for both X and Z errors
        DsDaSecondInv,
    }

    impl Purification {
        pub fn from(pur_type: PurType, interface: Interface) -> Self {
            Purification {
                purification_type: pur_type,
                qnic_interface: interface,
            }
        }
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct EntanglementSwapping {
        pub self_qnic_interfaces: Vec<Interface>,
        pub remote_qnic_interfaces: Vec<Interface>,
    }

    impl EntanglementSwapping {
        pub fn new() -> Self {
            EntanglementSwapping {
                self_qnic_interfaces: vec![],
                remote_qnic_interfaces: vec![],
            }
        }
        pub fn from(
            qnic_interfaces: Vec<Interface>,
            remote_qnic_interfaces: Vec<Interface>,
        ) -> Self {
            EntanglementSwapping {
                self_qnic_interfaces: qnic_interfaces,
                remote_qnic_interfaces: remote_qnic_interfaces,
            }
        }

        pub fn add_qnic_interface(&mut self, qnic_interface: Interface) {
            self.self_qnic_interfaces.push(qnic_interface);
        }

        pub fn add_remote_qnic_interface(&mut self, qnic_interface: Interface) {
            self.remote_qnic_interfaces.push(qnic_interface);
        }
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Tomography {
        pub num_measure: u32,
        /// Should be deprecated in the near future
        pub owner_address: u32,
    }

    impl Tomography {
        pub fn new() -> Self {
            Tomography {
                num_measure: 0,
                owner_address: 0,
            }
        }

        pub fn from(num_measure: u32, owner_address: u32) -> Self {
            Tomography {
                num_measure: num_measure,
                owner_address: owner_address,
            }
        }

        pub fn add_num_measure(&mut self, num_measure: u32) {
            self.num_measure = num_measure;
        }
        pub fn add_owner_address(&mut self, owner_addr: u32) {
            self.owner_address = owner_addr;
        }
    }
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

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_action_clause() {
            let mut action = Action::new(None);
            let qgate = QGate::new(GateType::H, Qubit::new());
            let clause = ActionClauses::Gate(qgate.clone());
            action.add_action_clause(clause);
            assert_eq!(action.name, None);
            assert_eq!(action.clauses.len(), 1);
            assert_eq!(action.clauses[0], ActionClauses::Gate(qgate));
        }
    }
}
