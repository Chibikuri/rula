use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::net::IpAddr;

use super::action::*;
use super::condition::v1::ConditionClauses;
use super::condition::*;
use crate::qnic::QnicType;

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
    // pub default_rule: Option<Rule<T>>,
    /// List of rules stored in this RuleSet
    pub stages: Vec<Stage<T>>,
    /// To give index to the rules sequentially
    num_rules: u32,
}

// For generating RuleSet for simulator or real world devices,
// owner address will return integer value or ip address
// This might be deprecated in the future
// Note: This can be a generic type, but in the generator, the RuleSet is called
// as a global singleton. For that purpose, this enum is defined.
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum AddressKind {
    // For connection to the simulator
    IntegerKind(u32),
    IpKind(IpAddr),
}

impl<T> RuleSet<T> {
    pub fn new(name: &str) -> Self {
        RuleSet {
            name: name.to_string(),
            id: generate_id(),
            owner_addr: None,
            // default_rule: None,
            stages: vec![],
            num_rules: 0,
        }
    }

    pub fn update_name(&mut self, name: &str) {
        self.name = name.to_string();
    }

    pub fn update_owner_addr(&mut self, owner_addr: Option<AddressKind>) {
        self.owner_addr = owner_addr;
    }

    // pub fn add_default_rule(&mut self, rule: Option<Rule<T>>) {
    //     self.default_rule = rule;
    // }

    pub fn add_stage(&mut self, mut stage: Stage<T>) {
        stage.update_id(self.num_rules);
        self.stages.push(stage);
        self.num_rules += 1;
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Stage<T> {
    pub rules: Vec<Rule<T>>,
    stage_id: u32,
}

impl<T> Stage<T> {
    pub fn new() -> Self {
        Stage {
            rules: vec![],
            stage_id: 0,
        }
    }

    pub fn update_id(&mut self, id: u32) {
        self.stage_id = id;
    }

    pub fn add_rule(&mut self, rule: Rule<T>) {
        self.rules.push(rule);
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct InterfaceInfo {
    // This could be ip addr in the future
    partner_addr: Option<PartnerAddr>,
    qnic_id: Option<u32>,
    qnic_type: Option<QnicType>,
    qnic_address: Option<IpAddr>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum PartnerAddr {
    IpAddr(IpAddr),
    Number(u32),
}

impl InterfaceInfo {
    pub fn new(
        partner_addr: Option<PartnerAddr>,
        qnic_id: Option<u32>,
        qnic_type: Option<QnicType>,
        qnic_address: Option<IpAddr>,
    ) -> Self {
        InterfaceInfo {
            partner_addr: partner_addr,
            qnic_id: qnic_id,
            qnic_type: qnic_type,
            qnic_address: qnic_address,
        }
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
    // #[deprecated(since = "0.2.0", note = "old version ruleset")]
    pub qnic_interfaces: HashMap<String, InterfaceInfo>,
    /// A list of conditions to be met
    pub condition: Condition,
    /// A list of actions to be acted
    pub action: Action<T>,
    /// If this is the final rule or not
    pub is_finalized: bool,
}

impl<T> Rule<T> {
    pub fn new(name: &str) -> Self {
        Rule {
            name: String::from(name),
            qnic_interfaces: HashMap::new(),
            id: 0,
            shared_tag: 0,
            condition: Condition::new(None),
            action: Action::new(None),
            is_finalized: false,
        }
    }
    pub fn set_condition(&mut self, condition: Condition) {
        self.condition = condition;
    }
    pub fn set_action(&mut self, action: Action<T>) {
        self.action = action;
    }

    pub fn add_condition_clause(&mut self, condition_clause: ConditionClauses) {
        self.condition.add_condition_clause(condition_clause);
    }

    pub fn add_action_clause(&mut self, action_clause: T) {
        self.action.add_action_clause(action_clause);
    }
    pub fn add_interface(&mut self, interface_name: &str, interface: InterfaceInfo) {
        self.qnic_interfaces
            .insert(interface_name.to_string(), interface);
    }
    pub fn update_id(&mut self, new_id: u32) {
        self.id = new_id;
    }
    pub fn update_shared_tag(&mut self, tag: u32) {
        self.shared_tag = tag;
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

    #[test]
    fn test_ruleset_new() {
        let ruleset = RuleSet::<Action<ActionClauses>>::new("test");
        assert_eq!(ruleset.name, String::from("test"));
        assert_eq!(ruleset.id.to_string(), "1234567890");
        assert_eq!(ruleset.stages.len(), 0);
    }

    #[test]
    fn test_ruleset_add_rule() {
        let mut ruleset = RuleSet::<ActionClauses>::new("test");
        let rule = Rule::new("rule1");
        let rule2 = Rule::new("rule2");
        let mut stage = Stage::new();
        stage.add_rule(rule);
        stage.add_rule(rule2);
        ruleset.add_stage(stage);
        assert_eq!(ruleset.stages[0].rules.len(), 2);
        assert_eq!(ruleset.stages[0].rules[0].name, "rule1");
        assert_eq!(ruleset.stages[0].rules[1].name, "rule2");
    }

    #[test]
    fn test_rule_new() {
        let rule = Rule::<ActionClauses>::new("test");
        assert_eq!(rule.name, String::from("test"));
        assert_eq!(rule.id, 0);
        assert_eq!(rule.condition, Condition::new(None));
        assert_eq!(rule.action, Action::<ActionClauses>::new(None));
        assert_eq!(rule.shared_tag, 0);
        assert_eq!(rule.is_finalized, false);
    }

    #[test]
    fn test_rule_add_condition_and_action() {
        let mut rule = Rule::<ActionClauses>::new("test");
        let condition = Condition::new(None);
        let action = Action::new(None);
        rule.set_condition(condition);
        rule.set_action(action);
        assert_eq!(rule.condition, Condition::new(None));
        assert_eq!(rule.action, Action::new(None));
    }
}
