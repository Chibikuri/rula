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

#[derive(Serialize, Deserialize)]
pub enum Awaitable {
    Test,
}

#[derive(Serialize, Deserialize)]
pub struct Action {
    pub name: String,
    pub operatables: Vec<Operatable>,
}

#[derive(Serialize, Deserialize)]
pub enum Operatable {
    Test,
}

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
