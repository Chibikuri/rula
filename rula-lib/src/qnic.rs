use super::message::Message;
use super::prelude::*;
use super::qubit::QubitInterface;
use super::result::{MeasResult, QResult};
use super::ruleset::condition::v1::*;
use super::RuleVec;
use mock_components::software::mock_routing_daemon::MockQnicRoutingTable;
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::HashMap;
use std::net::{IpAddr, Ipv4Addr};

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum QnicType {
    QnicE,
    QnicP,
    QnicRp,
    QnicN, // place holder
}

#[derive(Serialize, Clone, Debug, PartialEq)]
pub struct QnicInterface {
    name: String,
    pub address: IpAddr,
    qubit_interfaces: RefCell<HashMap<String, Box<QubitInterface>>>,
    // HashMap for tracking qubit usage
    qubit_busy: RefCell<HashMap<String, bool>>,
    messages: HashMap<String, Message>,
    // This will be deprecated
    pub routing_table: MockQnicRoutingTable,
}

impl QnicInterface {
    pub fn generate_mock_interface(interface_name: &str, num_qubits: u32) -> Self {
        let mut mock_table = MockQnicRoutingTable::new();
        mock_table.generate_mock_table();
        QnicInterface {
            name: String::from(interface_name),
            address: IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)),
            qubit_interfaces: RefCell::new(Self::generate_mock_qubit_interface(num_qubits)),
            qubit_busy: RefCell::new(Self::generate_mock_qubit_usage(num_qubits)),
            messages: HashMap::new(),
            routing_table: mock_table,
        }
    }

    fn generate_mock_qubit_interface(num_qubits: u32) -> HashMap<String, Box<QubitInterface>> {
        let mut qubit_table = HashMap::new();
        for i in 0..num_qubits {
            let mock_qubit_interface: QubitInterface = QubitInterface::gen_mock();
            qubit_table.insert(format!("qubit_{}", i), Box::new(mock_qubit_interface));
        }
        qubit_table
    }

    fn generate_mock_qubit_usage(num_qubits: u32) -> HashMap<String, bool> {
        let mut qubit_busy_table = HashMap::new();
        for i in 0..num_qubits {
            qubit_busy_table.insert(format!("qubit_{}", i), false);
        }
        qubit_busy_table
    }

    pub fn place_holder() -> Self {
        QnicInterface {
            name: String::from(""),
            address: IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)),
            qubit_interfaces: RefCell::new(HashMap::new()),
            qubit_busy: RefCell::new(HashMap::new()),
            messages: HashMap::new(),
            routing_table: MockQnicRoutingTable::new(),
        }
    }

    pub async fn request_resource(&self, number: &u32, partner_addr: &IpAddr) -> QubitInterface {
        let mut available_qubit = String::new();
        for (name, busy) in self.qubit_busy.borrow().iter() {
            if !busy {
                available_qubit = name.to_string();
                break;
            }
        }
        self.qubit_interfaces
            .borrow()
            .get(&available_qubit)
            .expect("No qubit available for now")
            .set_busy();
        *self
            .qubit_interfaces
            .borrow()
            .get(&available_qubit)
            .expect("No qubit found")
            .to_owned()
    }

    pub fn __static__request_resource(
        &self,
        rules: RuleVec,
        number: u32,
        partner_addr: IpAddr,
        // add fidelity parameter
    ) -> QubitInterface {
        for rule in &*rules.borrow_mut() {
            rule.borrow_mut()
                .add_condition_clause(ConditionClauses::EnoughResource(EnoughResource::new(
                    number,
                    partner_addr,
                    Some(0.0),
                    Some(__get_interface_info(&self.name)),
                )));
        }
        QubitInterface::new()
    }

    pub async fn get_message(&self, src: &IpAddr) -> Message {
        let mut dest = QnicInterface::place_holder();
        Message::new(
            "",
            src,
            &dest.address,
            QResult {
                result: MeasResult {
                    qubit_address: 0,
                    output: "00".to_string(),
                },
                generated_actions: vec![],
            },
        )
    }

    pub fn __static__get_message(&self, rules: RuleVec, src: IpAddr) -> Message {
        let mut dest = QnicInterface::place_holder();
        // Should this be wait?
        for rule in &*rules.borrow_mut() {
            rule.borrow_mut()
                .add_condition_clause(ConditionClauses::Wait)
        }
        Message::new(
            "",
            &src,
            &dest.address,
            QResult {
                result: MeasResult {
                    qubit_address: 0,
                    output: "00".to_string(),
                },
                generated_actions: vec![],
            },
        )
    }
    pub async fn get_qubit_by_partner(&self, src: &IpAddr, qindex: &u32) -> QubitInterface {
        *self
            .qubit_interfaces
            .borrow()
            .get("")
            .expect("Unable to find a qubit")
            .to_owned()
    }

    pub fn __static__get_qubit_by_partner(
        &self,
        _: RuleVec,
        src: IpAddr,
        qindex: u32,
    ) -> QubitInterface {
        *self
            .qubit_interfaces
            .borrow()
            .get("qubit_0")
            .expect("Unable to find a qubit")
            .to_owned()
    }
    pub async fn get_partner_by_hop(&self, distance: &u64) -> IpAddr {
        // access to the routing table
        self.routing_table
            .get_interface_by_distance(*distance)
            .clone()
    }

    pub fn __static__get_partner_by_hop(&self, _: RuleVec, distance: u64) -> IpAddr {
        self.routing_table
            .get_interface_by_distance(distance)
            .clone()
    }
}
