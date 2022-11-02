use rula_parser::parser::ast::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct RuleMeta {
    watched_values: HashMap<String, Watchable>,
}

impl RuleMeta {
    pub fn new(watched_values: HashMap<String, Watchable>) -> Self {
        RuleMeta {
            watched_values: watched_values,
        }
    }
    pub fn place_holder() -> Self {
        RuleMeta {
            watched_values: HashMap::new(),
        }
    }
    pub fn insert_watch_value(&mut self, name: String, watchable: Watchable) {
        self.watched_values.insert(name, watchable);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Watchable {
    Expr(Expr),
}
