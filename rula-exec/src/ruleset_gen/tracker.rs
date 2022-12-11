use super::ruleset::RuleSet;

use std::collections::HashMap;

type NodeNumber = u32;
// Track all global state in generation
#[derive(Debug, Clone, PartialEq)]
pub struct Tracker {
    pub rulesets: HashMap<NodeNumber, RuleSet>,
}

impl Tracker {
    pub fn new() -> Self {
        Tracker {
            rulesets: HashMap::new(),
        }
    }
    pub fn add_ruleset(&mut self, number: NodeNumber, ruleset: RuleSet) {
        self.rulesets.insert(number, ruleset);
    }
}
