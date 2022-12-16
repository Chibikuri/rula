// use crate::ruleset::ruleset::{InterfaceInfo, PartnerAddr};

// use super::message::Message;
// use super::qubit::QubitInterface;
// use super::result::{MeasResult, QResult};
// use super::ruleset::condition::v1::*;
// use super::RuleVec;
// use mock_components::software::mock_routing_daemon::MockQnicRoutingTable;
// use serde::{Deserialize, Serialize};
// use std::cell::RefCell;
// use std::collections::HashMap;
// use std::net::{IpAddr, Ipv4Addr};

// /// QnicType
// /// Enum for the type of Qnic
// /// - QnicE: Emit photon from memory
// /// - QnicP: Receive photon and store it to memory (or do local bsm)
// /// - QnicRp: Rp(Receiver Passive) used for MSM link
// #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
// pub enum QnicType {
//     QnicE,
//     QnicP,
//     QnicRp,
//     QnicN, // place holder
// }

// // Software side information
// #[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
// pub struct QnicInfo {
//     pub qnic_type: QnicType,
//     pub qnic_id: u32,
//     pub qnic_address: IpAddr,
//     pub num_qubit: u32,
// }

// impl QnicInfo {
//     pub fn new(qnic_type: QnicType, qnic_id: u32, qnic_address: IpAddr, num_qubit: u32) -> Self {
//         QnicInfo {
//             qnic_type: qnic_type,
//             qnic_id: qnic_id,
//             qnic_address: qnic_address,
//             num_qubit: num_qubit,
//         }
//     }
// }

// /// QnicInterface
// /// Actual Qnic should be manipulated through this interface structure
// /// Values:
// ///     name: InterfaceName (This is just a conceptual name defined in rula program)
// ///     address: Assumes each qnic interface has one ip address.
// ///     qubit_interfaces: The list of qubit's interface
// ///     qubit_busy: (deprecated)
// ///     messages: Messages come from other node to this qnic
// ///     routing table: Assume each qnic has routing table and knows proper destination
// #[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
// pub struct QnicInterface {
//     pub name: String,
//     pub index: u32,
//     pub address: IpAddr,
//     qubit_interfaces: RefCell<HashMap<String, Box<QubitInterface>>>,
//     qubit_busy: RefCell<HashMap<String, bool>>,
//     messages: HashMap<String, Message>,
//     routing_table: MockQnicRoutingTable,
// }

// impl QnicInterface {
//     pub fn generate_mock_interface(qnic_index: u32, interface_name: &str, num_qubits: u32) -> Self {
//         let mut mock_table = MockQnicRoutingTable::new();
//         mock_table.generate_mock_table();
//         QnicInterface {
//             name: String::from(interface_name),
//             index: qnic_index,
//             address: IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)),
//             qubit_interfaces: RefCell::new(Self::generate_mock_qubit_interface(num_qubits)),
//             qubit_busy: RefCell::new(Self::generate_mock_qubit_usage(num_qubits)),
//             messages: HashMap::new(),
//             routing_table: mock_table,
//         }
//     }

//     pub fn get_qnic_info(&self) -> QnicInfo {
//         QnicInfo::new(
//             QnicType::QnicE,
//             self.index,
//             self.address,
//             self.qubit_interfaces.borrow().len() as u32,
//         )
//     }

//     pub fn get_interface_info(&self, partner_addr: PartnerAddr) -> InterfaceInfo {
//         let qnic_info = self.get_qnic_info();
//         InterfaceInfo::new(
//             Some(partner_addr),
//             Some(qnic_info.qnic_id),
//             Some(qnic_info.qnic_type),
//             Some(qnic_info.qnic_address),
//         )
//     }

//     fn generate_mock_qubit_interface(num_qubits: u32) -> HashMap<String, Box<QubitInterface>> {
//         let mut qubit_table = HashMap::new();
//         for i in 0..num_qubits {
//             let mock_qubit_interface: QubitInterface = QubitInterface::gen_mock();
//             qubit_table.insert(format!("qubit_{}", i), Box::new(mock_qubit_interface));
//         }
//         qubit_table
//     }

//     fn generate_mock_qubit_usage(num_qubits: u32) -> HashMap<String, bool> {
//         let mut qubit_busy_table = HashMap::new();
//         for i in 0..num_qubits {
//             qubit_busy_table.insert(format!("qubit_{}", i), false);
//         }
//         qubit_busy_table
//     }

//     pub fn place_holder() -> Self {
//         QnicInterface {
//             name: String::from(""),
//             index: 0,
//             address: IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)),
//             qubit_interfaces: RefCell::new(HashMap::new()),
//             qubit_busy: RefCell::new(HashMap::new()),
//             messages: HashMap::new(),
//             routing_table: MockQnicRoutingTable::new(),
//         }
//     }

//     pub async fn request_resource(&self, number: &u32, partner_addr: &IpAddr) -> QubitInterface {
//         let mut available_qubit = String::new();
//         for (name, busy) in self.qubit_busy.borrow().iter() {
//             if !busy {
//                 available_qubit = name.to_string();
//                 break;
//             }
//         }
//         self.qubit_interfaces
//             .borrow()
//             .get(&available_qubit)
//             .expect("No qubit available for now")
//             .set_busy();
//         *self
//             .qubit_interfaces
//             .borrow()
//             .get(&available_qubit)
//             .expect("No qubit found")
//             .to_owned()
//     }

//     pub fn __static__request_resource(
//         &self,
//         rules: RuleVec,
//         number: u32,
//         partner_addr: IpAddr,
//         // add fidelity parameter
//     ) -> QubitInterface {
//         for rule in &*rules.borrow_mut() {
//             rule.borrow_mut()
//                 .add_condition_clause(ConditionClauses::EnoughResource(EnoughResource::new(
//                     number,
//                     partner_addr,
//                     Some(0.0),
//                     Some(self.get_interface_info(PartnerAddr::IpAddr(partner_addr))),
//                 )));
//         }
//         QubitInterface::new()
//     }

//     pub async fn get_message(&self, src: &IpAddr) -> Message {
//         let mut dest = QnicInterface::place_holder();
//         Message::new(
//             "",
//             src,
//             &dest.address,
//             QResult {
//                 result: MeasResult {
//                     qubit_address: 0,
//                     output: "00".to_string(),
//                 },
//                 generated_actions: vec![],
//             },
//         )
//     }

//     pub fn __static__get_message(&self, rules: RuleVec, src: IpAddr) -> Message {
//         let mut dest = QnicInterface::place_holder();
//         // Should this be wait?
//         for rule in &*rules.borrow_mut() {
//             rule.borrow_mut()
//                 .add_condition_clause(ConditionClauses::Wait)
//         }
//         Message::new(
//             "",
//             &src,
//             &dest.address,
//             QResult {
//                 result: MeasResult {
//                     qubit_address: 0,
//                     output: "00".to_string(),
//                 },
//                 generated_actions: vec![],
//             },
//         )
//     }
//     pub async fn get_qubit_by_partner(&self, src: &IpAddr, qindex: &u32) -> QubitInterface {
//         *self
//             .qubit_interfaces
//             .borrow()
//             .get("")
//             .expect("Unable to find a qubit")
//             .to_owned()
//     }

//     pub fn __static__get_qubit_by_partner(
//         &self,
//         _: RuleVec,
//         _src: IpAddr,
//         _qindex: u32,
//     ) -> QubitInterface {
//         *self
//             .qubit_interfaces
//             .borrow()
//             .get("qubit_0")
//             .expect("Unable to find a qubit")
//             .to_owned()
//     }
//     pub async fn get_partner_by_hop(&self, distance: &u64) -> IpAddr {
//         // access to the routing table
//         self.routing_table
//             .get_interface_by_distance(*distance)
//             .clone()
//     }

//     pub fn __static__get_partner_by_hop(&self, _: RuleVec, distance: u64) -> IpAddr {
//         self.routing_table
//             .get_interface_by_distance(distance)
//             .clone()
//     }
// }
