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

#[derive(Serialize, Deserialize)]
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
}

#[derive(Serialize, Deserialize)]
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

#[derive(Serialize, Deserialize)]
pub struct Condition {
    pub name: Option<String>,
    pub clauses: Vec<ConditionClauses>,
}

#[derive(Serialize, Deserialize)]
pub struct Action {
    //
    pub name: String,
    pub clauses: Vec<ActionClauses>,
}

trait ClauseTrait {
    // activate clause and check the status
    fn activate() {}
}

// Awaitable conditions that can be met in the future
#[derive(Serialize, Deserialize)]
pub enum ConditionClauses {
    /// Fidelity of the resource
    Fidelity(f64),
    /// The number of available resources in the QNIC
    Count,
    /// Trigger timer message
    Time(f64),
}

#[derive(Serialize, Deserialize)]
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

#[derive(Serialize, Deserialize)]
pub struct QGate {
    pub kind: GateType,
    pub target: Qubit,
}

#[derive(Serialize, Deserialize)]
pub enum GateType {
    X,
    Y,
    Z,
    H,
    Cx,
    Cz,
}

#[derive(Serialize, Deserialize)]
pub enum MeasBasis {
    X,
    Y,
    Z,
    U(f64, f64, f64),
}

#[derive(Serialize, Deserialize)]
pub struct Message {
    pub kind: String,
    pub src: IpAddr,
    pub dst: IpAddr,
    pub res: Box<Option<String>>,
}

#[derive(Serialize, Deserialize)]
pub struct Qubit {}

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
}
