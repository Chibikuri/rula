pub mod prelude {
    use super::message::Message;
    use super::qubit::QubitInterface;
    pub fn free(qubit: QubitInterface) {}
    pub fn send(message: Message) {}
}

pub mod message {
    use std::net::IpAddr;

    use crate::result::{QResult, MeasResult};

    use super::qnic::QnicInterface;

    pub fn Message(kind: &str, src_qnic: &QnicInterface, dst_qnic: &QnicInterface) -> Message {
        Message::new(kind, src_qnic, dst_qnic, QResult {result: MeasResult{qubit_address: 0, output: "00".to_string()}  })
    }
    pub struct Message {
        pub kind: String,
        pub src: IpAddr,
        pub dst: IpAddr,
        pub body: QResult
    }
    impl Message {
        pub fn new(kind: &str, src: &QnicInterface, dst: &QnicInterface, result: QResult) -> Message {
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

    pub fn bsm(q1: &mut QubitInterface, q2: &mut QubitInterface) -> String {
        String::from("result")
    }
}

pub mod qnic {
    use crate::result::{QResult, MeasResult};

    use super::message::Message;
    use super::qubit::QubitInterface;
    use std::collections::HashMap;
    use std::hash::Hash;
    use std::net::{IpAddr, Ipv4Addr};

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
            &mut self,
            number: u32,
            qnic_interface: &QnicInterface,
        ) -> &mut QubitInterface {
            self.qubit_interfaces
                .get_mut("")
                .expect("Unable to find qubit")
        }
        pub async fn get_message(&self, src: &mut QnicInterface) -> Message {
            let mut dest = QnicInterface::place_holder();
            Message::new("", src, &mut dest, QResult{result: MeasResult{qubit_address: 0, output: "00".to_string()}})
        }
        pub fn get_qubit_by_partner(&self, src: IpAddr, qindex: u32) -> QubitInterface {
            QubitInterface {}
        }
        pub fn get_partner_by_hop(&self, distance: u64) -> &QnicInterface {
            // access to the routing table
            self.routing_table.get(&distance).unwrap()
        }
    }
}
pub mod qubit {
    pub struct QubitInterface {}
    impl QubitInterface {
        pub async fn ready(&self) -> bool {
            true
        }
        pub async fn x(&self){

        }
        pub async fn z(&self){}
    }
}

pub mod result {
    use super::qubit::QubitInterface;
    pub fn Result(qubit: &mut QubitInterface) -> QResult {
        QResult {result: MeasResult { qubit_address: 0, output: "00".to_string() }}
    }
    pub struct QResult {
        pub result: MeasResult
    }

    pub struct MeasResult{
        pub qubit_address: u32,
        pub output: String, 
    }

    impl QResult {
        pub fn add_tag(&mut self, tag: &str) -> &mut Self {
            self
        }
        pub fn add_result(&mut self, result: String) {}
    }
    impl MeasResult{
        pub fn get_output(&self) -> &str{
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

    #[async_trait]
    pub trait Rulable {
        async fn condition(&self) -> bool;
        fn post_process(&self);
        fn execute(&self);
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Argument {
        pub filled: bool,
        pub value: ArgVal,
        pub type_hint: TypeHint,
    }

    // TODO: Argument func should be more flex
    impl Argument {
        pub fn init() -> Self {
            // FIXME: Initializing "" would not be a good idea
            Argument {
                filled: false,
                value: ArgVal::PlaceHolder,
                type_hint: TypeHint::Unknown,
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

        pub fn eval_unsigned_int64(&self) -> u64 {
            self.value.eval_unsigned_int64()
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum ArgVal {
        Str(String),
        Integer64(i64),
        Float64(f64),
        UnsignedInteger64(u64),
        Boolean(bool),
        PlaceHolder,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum TypeHint {
        Str,
        Int64,
        UnsignedInt64,
        Float64,
        Boolean,
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
    }
}
