use serde::{Deserialize, Serialize};
use std::net::IpAddr;
use uuid::{uuid, Uuid};

fn generate_id() -> Uuid {
    if cfg!(test) {
        uuid!("67e55044-10b1-426f-9247-bb680e5fe0c8")
    } else {
        Uuid::new_v4()
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct RuleSet {
    /// name of this ruleset (Different from identifier, just for easiness)
    pub name: String,
    /// Unique identifier for thie RuleSet. (This could be kept in private)
    pub id: Uuid,
    /// Host address of this RuleSet
    pub owner: IpAddr,
    /// List of rules stored in this RuleSet
    pub rules: Vec<Rule>,
}

impl RuleSet {
    pub fn new(name: &str, host_ip: IpAddr) -> Self {
        RuleSet {
            name: String::from(name),
            id: generate_id(),
            owner: host_ip,
            rules: vec![],
        }
    }
    pub fn add_rule(&mut self, rule: Rule) {
        self.rules.push(rule);
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Rule {
    /// Name of this rule
    pub name: String,
    /// Identifier of this Rule
    pub id: Uuid,
    /// A list of conditions to be met
    pub conditions: Vec<Condition>,
    /// A list of actions to be acted
    pub actions: Vec<Action>,
}

impl Rule {
    pub fn new(name: &str) -> Self {
        Rule {
            name: String::from(name),
            id: generate_id(),
            conditions: vec![],
            actions: vec![],
        }
    }
    pub fn add_condition(&mut self, condition: Condition) {
        self.conditions.push(condition);
    }
    pub fn add_action(&mut self, action: Action) {
        self.actions.push(action);
    }
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

trait ClauseTrait {
    // activate clause and check the status
    fn activate() {}
}

// Awaitable conditions that can be met in the future
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum ConditionClauses {
    /// Fidelity of the resource
    Fidelity(f64),
    /// The number of available resources in the QNIC
    Count,
    /// Trigger timer message
    Time(f64),
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
pub struct Qubit {}

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
        let ruleset = RuleSet::new("test", IpAddr::V4(Ipv4Addr::new(192, 168, 0, 1)));
        assert_eq!(ruleset.name, String::from("test"));
        assert_eq!(
            ruleset.id.to_string(),
            "67e55044-10b1-426f-9247-bb680e5fe0c8"
        );
        assert_eq!(ruleset.owner, IpAddr::V4(Ipv4Addr::new(192, 168, 0, 1)));
        assert_eq!(ruleset.rules.len(), 0);
    }

    #[test]
    fn test_ruleset_add_rule() {
        let mut ruleset = RuleSet::new("test", IpAddr::V4(Ipv4Addr::new(192, 168, 0, 1)));
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
        assert_eq!(rule.id.to_string(), "67e55044-10b1-426f-9247-bb680e5fe0c8");
        assert_eq!(rule.conditions.len(), 0);
        assert_eq!(rule.actions.len(), 0);
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

    // #[test]
}
