pub mod prelude {
    use super::qnic::QnicInterface;
    use std::collections::HashMap;
    use std::net::IpAddr;
    use std::net::Ipv4Addr;
    use tokio::sync::Mutex;

    use crate::ruleset::condition::v1::ConditionClauses;
    use crate::ruleset::ruleset::InterfaceInfo;

    use super::message::Message;
    use super::qubit::QubitInterface;
    pub fn free(qubit: &QubitInterface) {}
    pub fn __static__free(qubit: QubitInterface) {}
    pub fn send(message: &Message) {}
    pub fn __static__send(message: Message) {}
    pub fn __get_interface_info(name: String) -> InterfaceInfo {
        InterfaceInfo::new(None, None, None)
    }

    pub fn __static__request_resource<'a>(
        interface_info: &InterfaceInfo,
        num_resource: u64,
        partner_addr: IpAddr,
    ) -> ConditionClauses<'a> {
        ConditionClauses::Wait
    }
}

pub mod message {
    use crate::{
        result::{MeasResult, QResult},
        ruleset::{
            action::v2::ActionClauses,
            condition::{v1::ConditionClauses, Condition},
        },
    };
    use serde::Serialize;
    use std::net::IpAddr;

    pub fn Message<'a>(kind: &str, src_addr: &IpAddr, dst_addr: &IpAddr) -> Message<'a> {
        Message::new(
            kind,
            src_addr,
            dst_addr,
            QResult {
                result: MeasResult {
                    qubit_address: 0,
                    output: "00".to_string(),
                },
                generated_actions: vec![],
            },
        )
    }

    pub fn __static__Message(kind: &str, src_qnic_addr: IpAddr, dst_qnic_addr: IpAddr) -> Message {
        Message::new(
            kind,
            &src_qnic_addr,
            &dst_qnic_addr,
            QResult {
                result: MeasResult {
                    qubit_address: 0,
                    output: "00".to_string(),
                },
                generated_actions: vec![],
            },
        )
    }
    #[derive(Serialize, Clone, Debug, PartialEq)]
    pub struct Message<'a> {
        pub kind: String,
        pub src: IpAddr,
        pub dst: IpAddr,
        pub body: QResult,

        pub generated_conditions: Vec<ConditionClauses<'a>>,
        pub generated_actions: Vec<ActionClauses>,
    }
    impl<'a> Message<'a> {
        pub fn new(kind: &str, src: &IpAddr, dst: &IpAddr, result: QResult) -> Message<'a> {
            Message {
                kind: String::from(kind),
                src: src.clone(),
                dst: dst.clone(),
                body: result,

                generated_conditions: vec![],
                generated_actions: vec![],
            }
        }
        pub fn append_body(&mut self, result: &QResult) {}
        pub fn __static__append_body(&mut self, result: QResult) {}
    }
}

pub mod operation {
    use crate::qubit::QubitInterface;

    pub fn bsm(q1: &QubitInterface, q2: &QubitInterface) -> String {
        String::from("result")
    }
    pub fn __static__bsm(_q1: QubitInterface, q2: QubitInterface) -> String {
        String::from("static __result")
    }
}

pub mod qnic {
    use crate::qubit;
    use crate::result::{MeasResult, QResult};
    use crate::ruleset::condition::Condition;
    use crate::ruleset::condition::v1::ConditionClauses;

    use super::message::Message;
    use super::qubit::QubitInterface;
    use mock_components::software::mock_routing_daemon::MockQnicRoutingTable;
    use serde::Serialize;
    use std::borrow::Borrow;
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::hash::Hash;
    use std::net::{IpAddr, Ipv4Addr};

    #[derive(Serialize, Debug, PartialEq, Clone)]
    pub enum QnicType {
        QnicE,
        QnicP,
        QnicRp,
        QnicN, // place holder
    }

    #[derive(Serialize, Clone, Debug, PartialEq)]
    pub struct QnicInterface<'a> {
        name: String,
        pub address: IpAddr,
        qubit_interfaces: RefCell<HashMap<String, Box<QubitInterface<'a>>>>,
        // HashMap for tracking qubit usage
        qubit_busy: RefCell<HashMap<String, bool>>,
        messages: HashMap<String, Message<'a>>,
        // This will be deprecated
        pub routing_table: MockQnicRoutingTable,
    }
    impl<'a> QnicInterface<'a> {
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

        fn generate_mock_qubit_interface(
            num_qubits: u32,
        ) -> HashMap<String, Box<QubitInterface<'a>>> {
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

        pub async fn request_resource(&self, number: &u32, dest_addr: &IpAddr) -> QubitInterface<'a> {
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
                .expect("No qubit found").to_owned()
        }

        pub fn __static__request_resource(
            &self,
            number: u32,
            dest_addr: IpAddr,
        ) -> (QubitInterface, Option<ConditionClauses<'a>>) {
            (QubitInterface::new(), Some(ConditionClauses::Wait))
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

        pub fn __static__get_message(&self, src: IpAddr) -> (Message, Option<ConditionClauses<'a>>) {
            let mut dest = QnicInterface::place_holder();
            (Message::new(
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
            ),
        Some(ConditionClauses::Wait))
        }
        pub fn get_qubit_by_partner(&self, src: &IpAddr, qindex: &u32) -> QubitInterface {
            *self
                .qubit_interfaces
                .borrow()
                .get("")
                .expect("Unable to find a qubit")
                .to_owned()
        }

        pub fn __static__get_qubit_by_partner(
            &self,
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
        pub fn get_partner_by_hop(&self, distance: &u64) -> &IpAddr {
            // access to the routing table
            self.routing_table.get_interface_by_distance(*distance)
        }

        pub fn __static__get_partner_by_hop(&self, distance: u64) -> IpAddr {
            self.routing_table
                .get_interface_by_distance(distance)
                .clone()
        }
    }
}
pub mod qubit {
    use serde::Serialize;

    use crate::ruleset::{action::v2::ActionClauses, condition::v1::ConditionClauses};
    use std::borrow::{Borrow, BorrowMut};
    use std::cell::{Cell, RefCell};

    #[derive(Serialize, Clone, Debug, PartialEq)]
    pub struct QubitInterface<'a> {
        pub busy: Cell<bool>,
        pub generated_conditions: Vec<ConditionClauses<'a>>,
        pub generated_actions: Vec<ActionClauses>,
    }
    impl<'a> QubitInterface<'a> {
        pub fn new() -> Self {
            QubitInterface {
                busy: Cell::new(false),
                generated_conditions: vec![],
                generated_actions: vec![],
            }
        }
        pub async fn ready(&self) -> bool {
            true
        }
        pub fn __static__ready(&self) -> bool {
            true
        }
        pub async fn x(&self) {}
        pub fn __static__x(&self) {}
        pub async fn z(&self) {}
        pub fn __static__z(&self) {}
        pub fn is_busy(&self) -> bool {
            self.busy.get()
        }
        pub fn set_busy(&self) {
            self.busy.set(true);
        }
        pub fn gen_mock() -> Self {
            QubitInterface {
                busy: Cell::new(false),
                generated_conditions: vec![],
                generated_actions: vec![],
            }
        }
    }
}

pub mod result {
    use crate::ruleset::{action::v2::ActionClauses, condition::v1::ConditionClauses};

    use super::qubit::QubitInterface;
    use serde::Serialize;
    pub fn Result(qubit: &QubitInterface) -> QResult {
        QResult {
            result: MeasResult {
                qubit_address: 0,
                output: "00".to_string(),
            },
            generated_actions: vec![],
        }
    }

    pub fn __static__Result(ubit: QubitInterface) -> QResult {
        QResult {
            result: MeasResult {
                qubit_address: 0,
                output: "00".to_string(),
            },
            generated_actions: vec![],
        }
    }

    #[derive(Serialize, Clone, Debug, PartialEq)]
    pub struct QResult {
        pub result: MeasResult,

        pub generated_actions: Vec<ActionClauses>,
    }

    #[derive(Serialize, Clone, Debug, PartialEq)]
    pub struct MeasResult {
        pub qubit_address: u32,
        pub output: String,
    }

    impl QResult {
        pub fn add_tag(&mut self, tag: &str) -> &mut Self {
            self
        }
        pub fn __static__add_tag(&mut self, tag: &str) -> &mut Self {
            self
        }
        pub fn add_result(&mut self, result: &String) {}
        pub fn __static__add_result(&mut self, result: String) {}
    }
    impl MeasResult {
        pub fn get_output(&self) -> &str {
            &self.output
        }
        pub fn __static__get_output(&self) -> &str {
            &self.output
        }
    }
}
pub mod time {
    pub fn time() {}
}

pub mod sync {
    pub async fn awaitable_comp<T>(lhs: T, comp_op: String, rhs: T) {}
}

pub mod rule {
    use async_trait::async_trait;

    use crate::ruleset::action::v2::ActionClauses;
    use crate::ruleset::ruleset::RuleSet;

    #[async_trait]
    pub trait Rulable {
        async fn condition(&self) -> bool;
        fn post_process(&self);
        async fn execute(&self);
        fn gen_ruleset(&self, ruleset: &mut RuleSet<ActionClauses>);
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Argument {
        pub filled: bool,
        pub value: ArgVal,
        pub type_hint: LibTypeHint,
    }

    // TODO: Argument func should be more flex
    impl Argument {
        pub fn init() -> Self {
            // FIXME: Initializing "" would not be a good idea
            Argument {
                filled: false,
                value: ArgVal::PlaceHolder,
                type_hint: LibTypeHint::Unknown,
            }
        }

        pub fn eval_str(&self) -> String {
            self.value.eval_str()
        }

        pub fn eval_float64(&self) -> f64 {
            self.value.eval_float64()
        }

        pub fn eval_int64(&self) -> i64 {
            self.value.eval_int64()
        }

        pub fn eval_bool(&self) -> bool {
            self.value.eval_bool()
        }

        // pub fn eval_u64_vec(&self) -> Vec<u>

        pub fn eval_unsigned_int64(&self) -> u64 {
            self.value.eval_unsigned_int64()
        }

        pub fn add_argument(&mut self, value: ArgVal, type_hint: LibTypeHint) {
            self.filled = true;
            self.value = value;
            self.type_hint = type_hint;
        }

        pub fn resolved(&self) -> bool {
            self.filled
        }
    }

    // TODO: Should think of using generics here for vector contents,
    // But this makes another complexity for generated code.
    // For now, this has so many vector types inside
    #[derive(Debug, Clone, PartialEq)]
    pub enum ArgVal {
        Str(String),
        Integer64(i64),
        Float64(f64),
        UnsignedInteger64(u64),
        Boolean(bool),
        StrVector(Vec<String>),
        I64Vector(Vec<i64>),
        F64Vector(Vec<f64>),
        U64Vector(Vec<u64>),
        BoolVector(Vec<bool>),
        PlaceHolder,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum LibTypeHint {
        Str,
        Inteter64,
        UnsignedInteger64,
        Float64,
        Boolean,
        Vector(Box<LibTypeHint>),
        Unknown,
    }

    impl ArgVal {
        pub fn eval_str(&self) -> String {
            match self {
                ArgVal::Str(string) => string.to_string(),
                _ => {
                    panic!("This needs to be string")
                }
            }
        }
        pub fn eval_int64(&self) -> i64 {
            match self {
                ArgVal::Integer64(int64) => int64.clone(),
                _ => {
                    panic!("This needs to be integer 64")
                }
            }
        }
        pub fn eval_unsigned_int64(&self) -> u64 {
            match self {
                ArgVal::UnsignedInteger64(unsigned64) => unsigned64.clone(),
                _ => panic!("This needs to be unsigned integer 64"),
            }
        }
        pub fn eval_float64(&self) -> f64 {
            match self {
                ArgVal::Float64(float64) => float64.clone(),
                _ => panic!("This needs to be float 64"),
            }
        }

        pub fn eval_bool(&self) -> bool {
            match self {
                ArgVal::Boolean(boolean) => boolean.clone(),
                _ => panic!("This needs to be a boolean"),
            }
        }

        pub fn eval_str_vec(&self) -> Vec<String> {
            match self {
                ArgVal::StrVector(str_vec) => str_vec.to_vec(),
                _ => panic!("This needs to be a vector"),
            }
        }

        pub fn eval_i64_vec(&self) -> Vec<i64> {
            match self {
                ArgVal::I64Vector(i64vec) => i64vec.to_vec(),
                _ => panic!("This needs to be integer 64 vector"),
            }
        }
        pub fn eval_f64_vec(&self) -> Vec<f64> {
            match self {
                ArgVal::F64Vector(f64vec) => f64vec.to_vec(),
                _ => panic!("This needs to be float 64 vector"),
            }
        }

        pub fn eval_u64vec(&self) -> Vec<u64> {
            match self {
                ArgVal::U64Vector(u64vec) => u64vec.to_vec(),
                _ => panic!("This needs to be unsigned integer 64 vector"),
            }
        }

        pub fn eval_bool_vec(&self) -> Vec<bool> {
            match self {
                ArgVal::BoolVector(bool_vec) => bool_vec.to_vec(),
                _ => panic!("This needs to be integer 64 vector"),
            }
        }
    }
}

pub mod ruleset {
    pub mod action;
    pub mod condition;
    pub mod ruleset;
}
