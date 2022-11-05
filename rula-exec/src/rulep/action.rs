use crate::wrapper::qnic_wrapper::QnicInterfaceWrapper;
use crate::wrapper::qubit_wrapper::QubitInterfaceWrapper;
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

#[deprecated(since = "0.2.0", note = "These actions are no longer valid.")]
pub mod v1 {
    /// Version 1 actions
    use super::*;
    use mock_components::hardware::qnic::QnicType;
    use std::net::Ipv4Addr;

    // old version of action clauses
    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub enum ActionClausesV1 {
        Purification(Purification),
        EntanglementSwapping(EntanglementSwapping),
        Wait(Wait),
        Tomography(Tomography),
    }

    /// `Purification action`
    /// Specify the type of purifications and attach the qnic interface information to it.
    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Purification {
        pub purification_type: PurType,
        pub qnic_interface: QnicInterfaceWrapper,
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
        pub fn from(pur_type: PurType, interface: QnicInterfaceWrapper) -> Self {
            Purification {
                purification_type: pur_type,
                qnic_interface: interface,
            }
        }
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct EntanglementSwapping {
        /// QNIC interface information placed inside the node
        pub self_qnic_interfaces: Vec<QnicInterfaceWrapper>,
        /// QNIC interface information of left and right nodes
        pub remote_qnic_interfaces: Vec<QnicInterfaceWrapper>,
    }

    impl EntanglementSwapping {
        pub fn new() -> Self {
            EntanglementSwapping {
                self_qnic_interfaces: vec![],
                remote_qnic_interfaces: vec![],
            }
        }
        pub fn from(
            qnic_interfaces: Vec<QnicInterfaceWrapper>,
            remote_qnic_interfaces: Vec<QnicInterfaceWrapper>,
        ) -> Self {
            EntanglementSwapping {
                self_qnic_interfaces: qnic_interfaces,
                remote_qnic_interfaces: remote_qnic_interfaces,
            }
        }

        pub fn add_qnic_interface(&mut self, qnic_interface: QnicInterfaceWrapper) {
            self.self_qnic_interfaces.push(qnic_interface);
        }

        pub fn add_remote_qnic_interface(&mut self, qnic_interface: QnicInterfaceWrapper) {
            self.remote_qnic_interfaces.push(qnic_interface);
        }
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Tomography {
        pub num_measure: u32,
        /// Should be deprecated in the near future
        pub qnic_interface: QnicInterfaceWrapper,
    }

    impl Tomography {
        pub fn new() -> Self {
            Tomography {
                num_measure: 0,
                qnic_interface: QnicInterfaceWrapper::place_holder(),
            }
        }

        pub fn from(num_measure: u32, qnic_interface: QnicInterfaceWrapper) -> Self {
            Tomography {
                num_measure: num_measure,
                qnic_interface: qnic_interface,
            }
        }

        pub fn add_num_measure(&mut self, num_measure: u32) {
            self.num_measure = num_measure;
        }
        // pub fn add_owner_address(&mut self, owner_addr: u32) {
        //     self.owner_address = owner_addr;
        // }
        pub fn add_interface(&mut self, qnic_interface: QnicInterfaceWrapper) {
            self.qnic_interface = qnic_interface;
        }
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Wait {
        pub qnic_interface: QnicInterfaceWrapper,
    }

    impl Wait {
        pub fn new(qnic_interface: QnicInterfaceWrapper) -> Self {
            Wait {
                qnic_interface: qnic_interface,
            }
        }
    }

    #[cfg(test)]
    pub mod tests {
        use super::*;

        #[test]
        fn test_purification_action() {
            let test_interface = QnicInterfaceWrapper::new(
                QnicType::QnicE,
                2,
                Some(IpAddr::V4(Ipv4Addr::new(192, 168, 0, 1))),
            );
            let pur_action = Purification::from(PurType::Double, test_interface);
            assert_eq!(pur_action.purification_type, PurType::Double);
            assert_eq!(pur_action.qnic_interface.qnic_type, QnicType::QnicE);
            assert_eq!(pur_action.qnic_interface.qnic_id, 2);
            assert_eq!(
                pur_action.qnic_interface.qnic_address.unwrap().to_string(),
                "192.168.0.1"
            );
        }

        #[test]
        fn test_swapping_action() {
            let test_self_interfaces = vec![
                QnicInterfaceWrapper::new(
                    QnicType::QnicE,
                    1,
                    Some(IpAddr::V4(Ipv4Addr::new(192, 168, 0, 1))),
                ),
                QnicInterfaceWrapper::new(
                    QnicType::QnicP,
                    2,
                    Some(IpAddr::V4(Ipv4Addr::new(192, 168, 0, 2))),
                ),
            ];
            let test_remote_interfaces = vec![
                QnicInterfaceWrapper::new(
                    QnicType::QnicE,
                    0,
                    Some(IpAddr::V4(Ipv4Addr::new(192, 168, 1, 1))),
                ),
                QnicInterfaceWrapper::new(
                    QnicType::QnicP,
                    0,
                    Some(IpAddr::V4(Ipv4Addr::new(192, 168, 2, 1))),
                ),
            ];
            let swapping_action =
                EntanglementSwapping::from(test_self_interfaces, test_remote_interfaces);
            assert_eq!(
                swapping_action.self_qnic_interfaces[0].qnic_type,
                QnicType::QnicE
            );
            assert_eq!(swapping_action.self_qnic_interfaces[0].qnic_id, 1);
            assert_eq!(
                swapping_action.self_qnic_interfaces[0]
                    .qnic_address
                    .unwrap()
                    .to_string(),
                "192.168.0.1"
            );
            assert_eq!(
                swapping_action.self_qnic_interfaces[1].qnic_type,
                QnicType::QnicP
            );
            assert_eq!(swapping_action.self_qnic_interfaces[1].qnic_id, 2);
            assert_eq!(
                swapping_action.self_qnic_interfaces[1]
                    .qnic_address
                    .unwrap()
                    .to_string(),
                "192.168.0.2"
            );

            assert_eq!(
                swapping_action.remote_qnic_interfaces[0].qnic_type,
                QnicType::QnicE
            );
            assert_eq!(swapping_action.remote_qnic_interfaces[0].qnic_id, 0);
            assert_eq!(
                swapping_action.remote_qnic_interfaces[0]
                    .qnic_address
                    .unwrap()
                    .to_string(),
                "192.168.1.1"
            );
            assert_eq!(
                swapping_action.remote_qnic_interfaces[1].qnic_type,
                QnicType::QnicP
            );
            assert_eq!(swapping_action.remote_qnic_interfaces[1].qnic_id, 0);
            assert_eq!(
                swapping_action.remote_qnic_interfaces[1]
                    .qnic_address
                    .unwrap()
                    .to_string(),
                "192.168.2.1"
            );
        }
    }

    #[test]
    fn test_tomography_action() {
        let test_interface = QnicInterfaceWrapper::new(
            QnicType::QnicRp,
            0,
            Some(IpAddr::V4(Ipv4Addr::new(192, 168, 0, 1))),
        );
        let tomography = Tomography::from(8000, test_interface);
        assert_eq!(tomography.num_measure, 8000);
        assert_eq!(tomography.qnic_interface.qnic_type, QnicType::QnicRp);
        assert_eq!(tomography.qnic_interface.qnic_id, 0);
        assert_eq!(
            tomography.qnic_interface.qnic_address.unwrap().to_string(),
            "192.168.0.1"
        );
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
        Free(QubitInterfaceWrapper),
        /// Update the status of qubit
        Update(Update),
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct QGate {
        pub kind: QGateType,
        pub target: QubitInterfaceWrapper,
    }

    impl QGate {
        pub fn new(gate_kind: QGateType, target_qubit: QubitInterfaceWrapper) -> Self {
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
        pub target: QubitInterfaceWrapper,
    }

    impl Measure {
        pub fn new(basis: MeasBasis, target: QubitInterfaceWrapper) -> Self {
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
        pub interface_info: QnicInterfaceWrapper,
        pub qubit_info: QubitInterfaceWrapper,
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub enum MeasOutput {
        Zero,
        One,
    }
    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Update {
        pub target: QubitInterfaceWrapper,
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_action_clause() {
            let mut action = Action::new(None);
            let qgate = QGate::new(QGateType::H, QubitInterfaceWrapper::new());
            let clause = ActionClauses::Gate(qgate.clone());
            action.add_action_clause(clause);
            assert_eq!(action.name, None);
            assert_eq!(action.clauses.len(), 1);
            assert_eq!(action.clauses[0], ActionClauses::Gate(qgate));
        }

        #[test]
        fn test_measure_clause() {
            let measure_clause = Measure::new(MeasBasis::X, QubitInterfaceWrapper::new());
            assert_eq!(measure_clause.basis, MeasBasis::X);
            assert_eq!(measure_clause.target, QubitInterfaceWrapper::new())
        }
    }
}
