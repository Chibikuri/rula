use serde::{Deserialize, Serialize};
use std::net::{IpAddr, Ipv4Addr};
use uuid::{uuid, Uuid};

use super::condition::*;
use crate::network::qnic_wrapper::*;
use crate::rulep::action::Action;

fn generate_id() -> u128 {
    if cfg!(test) {
        1234567890
        // uuid!("67e55044-10b1-426f-9247-bb680e5fe0c8")
    } else {
        9876543210
        // Uuid::new_v4()
    }
}

// note: host addresses can only be filled in after
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct RuleSet<T> {
    /// name of this ruleset (Different from identifier, just for easiness)
    pub name: String,
    /// Unique identifier for thie RuleSet. (This could be kept in private)
    pub id: u128,
    /// Owner address can only be solved after the all network interface options are collected
    pub owner_addr: Option<AddressKind>,
    /// Default rule to be applied
    pub default_rule: Option<Rule<T>>,
    /// List of rules stored in this RuleSet
    pub rules: Vec<Rule<T>>,
    /// To give index to the rules sequentially
    rule_index: u32,
}

// For generating RuleSet for simulator or real world devices,
// owner address will return integer value or ip address
// This might be deprecated in the future
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum AddressKind {
    IntegerKind(i32),
    IpKind(IpAddr),
}

impl<T> RuleSet<T> {
    pub fn new(name: &str) -> Self {
        RuleSet {
            name: name.to_string(),
            id: generate_id(),
            owner_addr: None,
            default_rule: None,
            rules: vec![],
            rule_index: 0,
        }
    }

    pub fn update_name(&mut self, name: &str) {
        self.name = name.to_string();
    }

    pub fn update_owner_addr(&mut self, owner_addr: Option<AddressKind>) {
        self.owner_addr = owner_addr;
    }

    pub fn add_default_rule(&mut self, rule: Option<Rule<T>>) {
        self.default_rule = rule;
    }

    pub fn add_rule(&mut self, mut rule: Rule<T>) {
        rule.update_id(self.rule_index);
        self.rules.push(rule);
        self.rule_index += 1;
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Rule<T> {
    /// Name of this rule
    pub name: String,
    /// Identifier of this Rule
    pub id: u32,
    /// Identifier for partner rules
    pub shared_tag: u32,
    /// Interface information (will be deprecated)
    #[deprecated(since = "0.2.0", note = "old version ruleset")]
    pub qnic_interfaces: Option<Vec<Interface>>,
    /// A list of conditions to be met
    pub condition: Condition,
    /// A list of actions to be acted
    pub action: Action<T>,
    /// Next rule
    pub next_rule_id: u32,
    /// If this is the final rule or not
    pub is_finalized: bool,
}

impl<T> Rule<T> {
    pub fn new(name: &str) -> Self {
        Rule {
            name: String::from(name),
            qnic_interfaces: None,
            id: 0,
            shared_tag: 0,
            condition: Condition::new(None),
            action: Action::new(None),
            next_rule_id: 0,
            is_finalized: false,
        }
    }
    pub fn set_condition(&mut self, condition: Condition) {
        self.condition = condition;
    }
    pub fn add_action(&mut self, action: Action<T>) {
        self.action = action;
    }
    pub fn update_id(&mut self, new_id: u32) {
        self.id = new_id;
    }
    pub fn update_shared_tag(&mut self, tag: u32) {
        self.shared_tag = tag;
    }
    pub fn update_next_rule_id(&mut self, next_rule_id: u32) {
        self.next_rule_id = next_rule_id;
    }
    pub fn update_finalized(&mut self, finalize: bool) {
        self.is_finalized = finalize;
    }
}

#[cfg(test)]
pub mod tests {
    use super::super::action::v2::ActionClauses;
    use super::super::action::Action;
    use super::*;
    use mock_components::hardware::qnic::QnicType;

    #[test]
    fn test_ruleset_new() {
        let ruleset = RuleSet::<Action<ActionClauses>>::new("test");
        assert_eq!(ruleset.name, String::from("test"));
        assert_eq!(ruleset.id.to_string(), "1234567890");
        assert_eq!(ruleset.rules.len(), 0);
    }

    #[test]
    fn test_ruleset_add_rule() {
        let mut ruleset = RuleSet::<ActionClauses>::new("test");
        let rule = Rule::new("rule1");
        ruleset.add_rule(rule);
        assert_eq!(ruleset.rules.len(), 1);
        assert_eq!(ruleset.rules[0].name, "rule1");
        let rule2 = Rule::new("rule2");
        ruleset.add_rule(rule2);
        assert_eq!(ruleset.rules.len(), 2);
        assert_eq!(ruleset.rules[1].name, "rule2");
    }

    #[test]
    fn test_rule_new() {
        let rule = Rule::<ActionClauses>::new("test");
        assert_eq!(rule.name, String::from("test"));
        assert_eq!(rule.id, 0);
        assert_eq!(rule.condition, Condition::new(None));
        assert_eq!(rule.action, Action::<ActionClauses>::new(None));
        assert_eq!(rule.next_rule_id, 0);
        assert_eq!(rule.shared_tag, 0);
        assert_eq!(rule.is_finalized, false);
    }

    #[test]
    fn test_rule_add_condition_and_action() {
        let mut rule = Rule::<ActionClauses>::new("test");
        let condition = Condition::new(None);
        let action = Action::new(None);
        rule.set_condition(condition);
        rule.add_action(action);
        assert_eq!(rule.condition, Condition::new(None));
        assert_eq!(rule.action, Action::new(None));
    }

    #[test]
    fn test_interface_place_holder() {
        let interface = Interface::place_holder();
        assert_eq!(interface.qnic_type, QnicType::QnicN);
        assert_eq!(interface.qnic_id, 0);
        assert_eq!(interface.qnic_address, Ipv4Addr::new(0, 0, 0, 0));
    }

    // #[test]
}
