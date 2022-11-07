use once_cell::sync::OnceCell;
use std::collections::HashMap;
use std::sync::Mutex;

// <Idenfitifier_name, its information>
static IDENT_TABLE: OnceCell<Mutex<HashMap<String, IdentInfo>>> = OnceCell::new();

#[derive(Debug, Clone, PartialEq)]
pub struct IdentInfo {
    // rule name that variable belongs
    belonging_rule: String,
}

// #[derive(Debug, Clone, PartialEq)]
// pub enum IdentifierType {
//     QnicInterface,
//     WatchedValue,
//     RuleArgument,
//     Other,
// }
