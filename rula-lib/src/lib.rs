pub mod message {
    pub struct Message {
        pub kind: String,
    }
}

pub mod operation {
    pub fn bsm() {}
}

pub mod qnic {
    use super::message::Message;
    use super::qubit::QubitInterface;
    use std::collections::HashMap;

    pub struct QnicInterface {
        qubit_interfaces: HashMap<String, QnicInterface>,
    }
    impl QnicInterface {
        pub fn place_holder() -> Self {
            QnicInterface {
                qubit_interfaces: HashMap::new(),
            }
        }
        pub async fn request_resource(&self) -> QubitInterface {
            QubitInterface {}
        }
        pub async fn get_message(&self) -> Message {
            Message {
                kind: "test".to_string(),
            }
        }
    }
}
pub mod qubit {
    pub struct QubitInterface {}
    impl QubitInterface {
        pub async fn ready(&self) -> bool {
            true
        }
    }
    pub fn free() {}
}

pub mod result {
    pub struct Result {}
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
        async fn action(&self);
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
