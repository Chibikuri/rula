use serde::{Deserialize, Serialize};

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
}
