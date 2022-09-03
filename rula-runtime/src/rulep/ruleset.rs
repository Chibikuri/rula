use serde::{Deserialize, Serialize};
use std::net::IpAddr;
use uuid::Uuid;

#[derive(Serialize, Deserialize)]
pub struct RuleSet {
    /// name of this ruleset (Different from identifier, just for easiness)
    pub name: String,
    /// Unique identifier for thie RuleSet. (This could be kept in private)
    pub id: Uuid,
    /// Host address of this RuleSet
    pub host_ip: IpAddr,
    /// List of rules stored in this RuleSet
    pub rules: Vec<Rule>,
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
