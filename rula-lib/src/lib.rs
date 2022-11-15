pub mod prelude {
    use super::message::Message;
    use super::qubit::QubitInterface;
    pub fn free(qubit: &QubitInterface) {}
    pub fn send(message: Message) {}
}

pub mod message {
    use super::qnic::QnicInterface;
    use crate::result::{MeasResult, QResult};
    use serde::{Deserialize, Serialize};
    use std::net::IpAddr;

    pub fn Message(kind: &str, src_qnic: &QnicInterface, dst_qnic: &QnicInterface) -> Message {
        Message::new(
            kind,
            src_qnic,
            dst_qnic,
            QResult {
                result: MeasResult {
                    qubit_address: 0,
                    output: "00".to_string(),
                },
            },
        )
    }
    #[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
    pub struct Message {
        pub kind: String,
        pub src: IpAddr,
        pub dst: IpAddr,
        pub body: QResult,
    }
    impl Message {
        pub fn new(
            kind: &str,
            src: &QnicInterface,
            dst: &QnicInterface,
            result: QResult,
        ) -> Message {
            Message {
                kind: String::from(kind),
                src: src.address,
                dst: dst.address,
                body: result,
            }
        }
        pub fn append_body(&mut self, result: QResult) {}
    }
}

pub mod operation {
    use crate::qubit::QubitInterface;

    pub fn bsm(q1: &QubitInterface, q2: &QubitInterface) -> String {
        String::from("result")
    }
}

pub mod qnic {
    use crate::result::{MeasResult, QResult};
    use crate::ruleset::action::v2::ActionClauses;
    use crate::ruleset::condition::v1::ConditionClauses;

    use super::message::Message;
    use super::qubit::QubitInterface;
    use serde::{Deserialize, Serialize};
    use std::collections::HashMap;
    use std::net::{IpAddr, Ipv4Addr};

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub enum QnicType {
        QnicE,
        QnicP,
        QnicRp,
        QnicN, // place holder
    }

    #[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
    pub struct QnicInterface {
        name: String,
        pub address: IpAddr,
        qubit_interfaces: HashMap<String, QubitInterface>,
        messages: HashMap<String, Message>,
        // This will be deprecated
        routing_table: HashMap<u64, QnicInterface>,
    }
    impl QnicInterface {
        pub fn place_holder() -> Self {
            QnicInterface {
                name: String::from(""),
                address: IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)),
                qubit_interfaces: HashMap::new(),
                messages: HashMap::new(),
                routing_table: HashMap::new(),
            }
        }
        pub async fn request_resource(
            &self,
            number: u32,
            qnic_interface: &QnicInterface,
        ) -> &QubitInterface {
            self.qubit_interfaces.get("").expect("Unable to find qubit")
        }

        pub async fn __static__request_resource(
            &self,
            condition_clauses: &mut Vec<ConditionClauses>,
            number: u32,
            qnic_interface: &QnicInterface,
        ) -> &QubitInterface {
            condition_clauses.push(ConditionClauses::Wait);
            self.qubit_interfaces.get("").expect("msg")
        }

        pub async fn get_message(&self, src: &QnicInterface) -> Message {
            let mut dest = QnicInterface::place_holder();
            Message::new(
                "",
                src,
                &mut dest,
                QResult {
                    result: MeasResult {
                        qubit_address: 0,
                        output: "00".to_string(),
                    },
                },
            )
        }

        pub async fn __static__get_message(
            &self,
            condition_clauses: &mut Vec<ConditionClauses>,
            src: &QnicInterface,
        ) -> Message {
            condition_clauses.push(ConditionClauses::Wait);
            let mut dest = QnicInterface::place_holder();
            Message::new(
                "",
                src,
                &mut dest,
                QResult {
                    result: MeasResult {
                        qubit_address: 0,
                        output: "00".to_string(),
                    },
                },
            )
        }
        pub fn get_qubit_by_partner(&self, src: IpAddr, qindex: u32) -> &QubitInterface {
            &QubitInterface {}
        }
        pub fn get_partner_by_hop(&self, distance: u64) -> &QnicInterface {
            // access to the routing table
            self.routing_table.get(&distance).unwrap()
        }

        pub fn __static__get_partner_by_hop(
            &self,
            _: &mut Vec<ConditionClauses>,
            distance: u64,
        ) -> &QnicInterface {
            self.routing_table.get(&distance).unwrap()
        }
    }
}
pub mod qubit {
    use serde::{Deserialize, Serialize};

    use crate::ruleset::condition::v1::ConditionClauses;

    #[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
    pub struct QubitInterface {}
    impl QubitInterface {
        pub fn new() -> Self {
            QubitInterface {}
        }
        pub async fn ready(&self) -> bool {
            true
        }
        pub async fn __static__ready(&self, condition_clauses: &mut Vec<ConditionClauses>) -> bool {
            condition_clauses.push(ConditionClauses::Wait);
            true
        }
        pub async fn x(&self) {}
        pub async fn z(&self) {}
    }
}

pub mod result {
    use super::qubit::QubitInterface;
    use serde::{Deserialize, Serialize};
    pub fn Result(qubit: &QubitInterface) -> QResult {
        QResult {
            result: MeasResult {
                qubit_address: 0,
                output: "00".to_string(),
            },
        }
    }

    #[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
    pub struct QResult {
        pub result: MeasResult,
    }

    #[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
    pub struct MeasResult {
        pub qubit_address: u32,
        pub output: String,
    }

    impl QResult {
        pub fn add_tag(&mut self, tag: &str) -> &mut Self {
            self
        }
        pub fn add_result(&mut self, result: String) {}
    }
    impl MeasResult {
        pub fn get_output(&self) -> &str {
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
