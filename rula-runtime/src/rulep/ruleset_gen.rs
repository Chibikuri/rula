use serde::{Deserialize, Serialize};
use serde_json::json;
use std::net::IpAddr;

#[derive(Serialize, Deserialize)]
pub struct RuleSet {
    /// name of this ruleset (Different from identifier, just for easiness)
    pub name: String,
    /// Unique identifier for thie RuleSet. (This could be kept in private)
    pub id: String,
    /// Host address of this RuleSet
    pub host_ip: IpAddr,
    /// List of rules stored in this RuleSet
    pub rules: Vec<Rule>,
}

#[derive(Serialize, Deserialize)]
pub struct Rule {
    pub name: String,
    pub id: u16,
    pub conditions: Vec<Condition>,
    pub actions: Vec<Action>,
}

#[derive(Serialize, Deserialize)]
pub struct Condition {
    pub name: String,
    pub awaitables: Vec<Awaitable>,
}

// Awaitable conditions that can be met in the future
#[derive(Serialize, Deserialize)]
pub enum Awaitable {
    /// Fidelity of the resource
    Fidelity,
    /// The number of available resources in the QNIC
    Count,
    /// Trigger timer message
    Time,
}

#[derive(Serialize, Deserialize)]
pub struct Action {
    pub name: String,
    pub operatables: Vec<Operatable>,
}

#[derive(Serialize, Deserialize)]
pub enum Operatable {
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
pub enum MeasBasis{
    X,
    Y, 
    Z,
    U(f64, f64, f64),
}

#[derive(Serialize, Deserialize)]
pub struct Message{
    pub kind: String,
    pub src: IpAddr,
    pub dst: IpAddr,
    pub res: Box<Option<String>>,
}

#[derive(Serialize, Deserialize)]
pub struct Qubit {}

impl RuleSet {
    pub fn new() {}
}
pub fn generate_ruleset() {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make_json() {
        let ruleset = generate_ruleset();
    }
}
