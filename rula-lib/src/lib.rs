pub mod message {
    pub struct Message {}
    pub async fn get_message() {}
}

pub mod operation {
    pub fn bsm() {}
}

pub mod qnic {
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
        async fn watch(&self) {}
        async fn condition(&self) {}
        // fn action(&self) {}
        fn post_process(&self) {}
        fn execute(&self) {}
    }

    pub struct Argument {
        pub filled: bool,
    }

    impl Argument {
        pub fn init() -> Self {
            Argument { filled: false }
        }
    }
}
