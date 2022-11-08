// This is autogenerated Rust program
use rula_lib as rula_std;
#[allow(unused)]
mod rula {
    use super::*;
    use async_trait::async_trait;
    use once_cell::sync::OnceCell;
    use rula_std::qnic::QnicInterface;
    use rula_std::rule::*;
    use std::collections::HashMap;
    use tokio::sync::Mutex;
    pub static INTERFACES: OnceCell<Mutex<HashMap<String, QnicInterface>>> = OnceCell::new();
    pub async fn initialize_interface() {
        assert!(INTERFACES.get().is_none());
        let initialize_interface = || Mutex::new(HashMap::new());
        INTERFACES.get_or_init(initialize_interface);
        let interface_list = INTERFACES.get().expect("Failed to get interface");
        for interface_name in vec!["qn0", "INTERFACE"] {
            interface_list
                .lock()
                .await
                .insert(interface_name.to_string(), QnicInterface::place_holder());
        }
    }
}
pub fn main() {
    rula::initialize_interface();
}
#[cfg(test)]
mod tests {
    use super::rula::*;
    use super::*;
    #[doc = "This is generated for entanglement_swapping.rula"]
    #[tokio::test]
    async fn test_interface() {
        assert!(INTERFACES.get().is_none());
        rula::initialize_interface();
        let interface = INTERFACES.get().expect("Failed to get interface table");
        assert!(interface.lock().await.contains_key("qn0"));
        assert!(interface.lock().await.contains_key("qn1"));
    }
}
