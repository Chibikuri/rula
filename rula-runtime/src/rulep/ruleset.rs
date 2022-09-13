use serde::{Deserialize, Serialize};
use std::net::{IpAddr, Ipv4Addr};
use uuid::{uuid, Uuid};

fn generate_id() -> Uuid {
    if cfg!(test) {
        uuid!("67e55044-10b1-426f-9247-bb680e5fe0c8")
    } else {
        Uuid::new_v4()
    }
}

// note: host addresses can only be filled in after
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct RuleSet {
    /// name of this ruleset (Different from identifier, just for easiness)
    pub name: String,
    /// Unique identifier for thie RuleSet. (This could be kept in private)
    pub id: Uuid,
    /// Owner address can only be solved after the all network interface options are collected
    pub owner_addr: Option<IpAddr>,
    /// List of rules stored in this RuleSet
    pub rules: Vec<Rule>,
    /// To give index to the rules sequentially
    rule_index: u32,
}

impl RuleSet {
    pub fn new(name: &str, host_name: &str) -> Self {
        RuleSet {
            name: String::from(name),
            id: generate_id(),
            owner_addr: None,
            rules: vec![],
            rule_index: 0,
        }
    }
    pub fn add_rule(&mut self, mut rule: Rule) {
        rule.update_id(self.rule_index);
        self.rules.push(rule);
        self.rule_index += 1;
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Rule {
    /// Name of this rule
    pub name: String,
    /// Identifier of this Rule
    pub id: u32,
    /// Identifier for partner rules
    pub shared_tag: u32,
    /// Interface information
    pub qnic_interfaces: Option<Vec<Interface>>,
    /// A list of conditions to be met
    pub conditions: Vec<Condition>,
    /// A list of actions to be acted
    pub actions: Vec<Action>,
    /// Next rule
    pub next_rule_id: u32,
    /// If this is the final rule or not
    pub is_finalized: bool,
}

impl Rule {
    pub fn new(name: &str) -> Self {
        Rule {
            name: String::from(name),
            qnic_interfaces: None,
            id: 0,
            shared_tag: 0,
            conditions: vec![],
            actions: vec![],
            next_rule_id: 0,
            is_finalized: false,
        }
    }
    pub fn add_condition(&mut self, condition: Condition) {
        self.conditions.push(condition);
    }
    pub fn add_action(&mut self, action: Action) {
        self.actions.push(action);
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

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Interface {
    // pub partner_address: IpAddr,
    pub qnic_type: QnicType,
    pub qnic_id: u32,
    pub qnic_address: IpAddr,
}

/// This should be a wrapper of actual QNIC interface
impl Interface {
    pub fn place_holder() -> Self {
        Interface {
            // partner_address: IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)),
            qnic_type: QnicType::QnicN,
            qnic_id: 0,
            qnic_address: IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum QnicType {
    QnicE,
    QnicP,
    QnicRp,
    QnicN, // place holder
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Condition {
    pub name: Option<String>,
    pub clauses: Vec<ConditionClauses>,
}

impl Condition {
    pub fn new(condition_name: Option<String>) -> Self {
        Condition {
            name: condition_name,
            clauses: vec![],
        }
    }

    pub fn add_condition_clause(&mut self, condition_clause: ConditionClauses) {
        self.clauses.push(condition_clause);
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Action {
    pub name: Option<String>,
    pub clauses: Vec<ActionClauses>,
}

impl Action {
    pub fn new(action_name: Option<String>) -> Self {
        Action {
            name: action_name,
            clauses: vec![],
        }
    }
    pub fn add_action_clause(&mut self, action_clause: ActionClauses) {
        self.clauses.push(action_clause);
    }
}

// Awaitable conditions that can be met in the future
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum ConditionClauses {
    /// The number of available resources in the QNIC
    EnoughResource(u32),
    /// Define the number of total measurements for tomography
    MeasureCount(u32),
    /// Fidelity of the resource
    Fidelity(f64),
    /// Just wait,
    Wait,
    /// Trigger timer message (Not implemented on quisp)
    Time(f64),
    // Comp,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum ActionClauses {
    /// Gate operations that can be applied immediately
    Gate(QGate),
    /// Measurement operations that takes calssical information from qubits
    Measure(MeasBasis),
    /// Send classical message from one place to another
    Send(Message),
    /// Update the status of qubit
    Update(Qubit),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct QGate {
    pub kind: GateType,
    pub target: Qubit,
}

impl QGate {
    pub fn new(gate_kind: GateType, target_qubit: Qubit) -> Self {
        QGate {
            kind: gate_kind,
            target: target_qubit,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum GateType {
    X,
    Y,
    Z,
    H,
    Cx,
    Cz,
    Rx(f64),
    Ry(f64),
    Rz(f64),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum MeasBasis {
    X,
    Y,
    Z,
    U(f64, f64, f64),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Message {
    pub kind: String,
    pub src: IpAddr,
    pub dst: IpAddr,
    pub res: Box<Option<String>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Qubit {
    // address: QubitAddress,
}

impl Qubit {
    pub fn new() -> Self {
        Qubit {}
    }
}

#[cfg(test)]
pub mod tests {
    use std::net::Ipv4Addr;

    use super::*;

    #[test]
    fn test_ruleset_new() {
        let ruleset = RuleSet::new("test", "test_host");
        assert_eq!(ruleset.name, String::from("test"));
        assert_eq!(
            ruleset.id.to_string(),
            "67e55044-10b1-426f-9247-bb680e5fe0c8"
        );
        assert_eq!(ruleset.rules.len(), 0);
    }

    #[test]
    fn test_ruleset_add_rule() {
        let mut ruleset = RuleSet::new("test", "test_host");
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
        let rule = Rule::new("test");
        assert_eq!(rule.name, String::from("test"));
        assert_eq!(rule.id, 0);
        assert_eq!(rule.conditions.len(), 0);
        assert_eq!(rule.actions.len(), 0);
        assert_eq!(rule.next_rule_id, 0);
        assert_eq!(rule.shared_tag, 0);
        assert_eq!(rule.is_finalized, false);
    }

    #[test]
    fn test_rule_add_condition_and_action() {
        let mut rule = Rule::new("test");
        let condition = Condition::new(None);
        let action = Action::new(None);
        rule.add_condition(condition);
        rule.add_action(action);
        assert_eq!(rule.conditions.len(), 1);
        assert_eq!(rule.actions.len(), 1);
    }

    #[test]
    fn test_action_clause() {
        let mut action = Action::new(None);
        let qgate = QGate::new(GateType::H, Qubit::new());
        let clause = ActionClauses::Gate(qgate.clone());
        action.add_action_clause(clause);
        assert_eq!(action.name, None);
        assert_eq!(action.clauses.len(), 1);
        assert_eq!(action.clauses[0], ActionClauses::Gate(qgate));
    }

    #[test]
    fn test_condition_clause() {
        let mut condition = Condition::new(None);
        let fidelity_clause = ConditionClauses::Fidelity(0.95);
        condition.add_condition_clause(fidelity_clause);
        assert_eq!(condition.name, None);
        assert_eq!(condition.clauses.len(), 1);
        assert_eq!(condition.clauses[0], ConditionClauses::Fidelity(0.95));
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
