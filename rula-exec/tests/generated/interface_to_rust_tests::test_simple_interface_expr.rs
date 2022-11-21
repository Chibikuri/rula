// This is autogenerated Rust program
use crate::rula_std::ruleset::ruleset::*;
use rula_lib as rula_std;
use rula_std::ruleset::action::v2::ActionClauses;
use std::fs;
#[allow(unused)]
mod rula {
    use super::*;
    use async_trait::async_trait;
    use log::warn;
    use once_cell::sync::OnceCell;
    use rula_std::message::*;
    use rula_std::operation::*;
    use rula_std::prelude::*;
    use rula_std::qnic::QnicInterface;
    use rula_std::qubit::QubitInterface;
    use rula_std::result::*;
    use rula_std::rule::*;
    use rula_std::ruleset::action::v2::ActionClauses as ActionClausesV2;
    use rula_std::ruleset::action::Action;
    use rula_std::ruleset::condition::v1::ConditionClauses;
    use rula_std::ruleset::condition::*;
    use rula_std::ruleset::ruleset::*;
    use serde::{Deserialize, Serialize};
    use std::borrow::BorrowMut;
    use std::cell::{Cell, RefCell};
    use std::collections::{HashMap, HashSet};
    use std::iter::FromIterator;
    use std::rc::Rc;
    use std::sync::Mutex as StdMutex;
    use tokio::sync::Mutex as TokioMutex;
    use tokio::time::{sleep, Duration};
    pub static INTERFACES: OnceCell<TokioMutex<HashMap<String, QnicInterface>>> = OnceCell::new();
    pub static STATIC_INTERFACES: OnceCell<StdMutex<HashMap<String, QnicInterface>>> =
        OnceCell::new();
    pub enum UnreadyRules<'a> {}
    impl<'a> UnreadyRules<'a> {
        pub fn check_arg_resolved(&self) -> Option<ReadyRules> {
            match &self {
                _ => {
                    panic!("No rule name found");
                }
            }
        }
        pub fn arg_list(&self) -> Vec<String> {
            match &self {
                _ => {
                    panic!("No rule name found");
                }
            }
        }
        pub fn resolve_argument(&mut self, arg_name: &str, argument: Argument) {
            match self {
                _ => {
                    panic!("No rule name found");
                }
            }
        }
        pub fn gen_ruleset(&mut self, ruleset: &mut RuleSet<'a, ActionClausesV2>) {
            match self {
                _ => {
                    panic!("No rule name found")
                }
            }
        }
    }
    pub enum ReadyRules {}
    pub async fn initialize_interface() {
        assert!(INTERFACES.get().is_none());
        let initialize_interface = || TokioMutex::new(HashMap::new());
        INTERFACES.get_or_init(initialize_interface);
        let interface_list = INTERFACES.get().expect("Failed to get interface");
        for interface_name in vec!["qn0", "INTERFACE"] {
            let mock_qnic = QnicInterface::generate_mock_interface(interface_name, 10);
            interface_list
                .lock()
                .await
                .insert(interface_name.to_string(), mock_qnic);
        }
    }
    pub fn initialize_static_interface() {
        assert!(STATIC_INTERFACES.get().is_none());
        let initialize_interface = || StdMutex::new(HashMap::new());
        STATIC_INTERFACES.get_or_init(initialize_interface);
        let interface_list = STATIC_INTERFACES.get().expect("Failed to get interface");
        for interface_name in vec!["qn0", "INTERFACE"] {
            let mock_qnic = QnicInterface::generate_mock_interface(interface_name, 10);
            interface_list
                .lock()
                .unwrap()
                .insert(interface_name.to_string(), mock_qnic);
        }
    }
}
pub async fn main() {
    rula::initialize_interface().await;
    rula::initialize_static_interface();
    let mut rulesets = vec![];
    for i in 0..1 {
        let mut ruleset = rula::RuleSetExec::init();
        ruleset.resolve_config(Box::new(&config), Some(i as usize));
        rulesets.push(static_ruleset);
    }
}
#[cfg(test)]
mod tests {
    use super::rula::*;
    use super::*;
    #[doc = "This is generated for entanglement_swapping.rula"]
    #[tokio::test]
    async fn test_interface() {
        assert!(INTERFACES.get().is_none());
        rula::initialize_interface().await;
        let interface = INTERFACES.get().expect("Failed to get interface table");
        assert!(interface.lock().await.contains_key("qn0"));
        assert!(interface.lock().await.contains_key("qn1"));
    }
    #[tokio::test]
    async fn run_main() {
        main().await;
        assert_eq!(1, 2);
    }
}
