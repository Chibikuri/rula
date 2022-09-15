use crate::network::qnic::Interface;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Condition {
    pub name: Option<String>,
    pub clauses: Vec<v1::ConditionClauses>,
}

impl Condition {
    pub fn new(condition_name: Option<String>) -> Self {
        Condition {
            name: condition_name,
            clauses: vec![],
        }
    }

    pub fn add_condition_clause(&mut self, condition_clause: v1::ConditionClauses) {
        self.clauses.push(condition_clause);
    }
}

mod v1 {
    use super::*;
    // Awaitable conditions that can be met in the future
    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub enum ConditionClauses {
        /// The number of available resources in the QNIC
        EnoughResource(EnoughResource),
        /// Define the number of total measurements for tomography
        MeasureCount(MeasureCount),
        /// Fidelity of the resource
        Fidelity(Fidelity),
        /// Just wait,
        Wait,
        /// Trigger timer message (Not implemented on quisp)
        Time(f64),
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct EnoughResource {
        name: String,
        pub num_required_resource: u32,
        pub required_fidelity: f64,
        pub qnic_interface: Interface,
    }

    impl EnoughResource {
        pub fn new() -> Self {
            EnoughResource {
                name: String::from("enough_resource"),
                num_required_resource: 0,
                required_fidelity: 0.0,
                qnic_interface: Interface::place_holder(),
            }
        }
        pub fn from(num_resource: u32, required_fidelity: f64, qnic_interface: Interface) -> Self {
            EnoughResource {
                name: String::from("enough_resource"),
                num_required_resource: num_resource,
                required_fidelity: required_fidelity,
                qnic_interface: qnic_interface,
            }
        }
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct MeasureCount {
        name: String,
        pub count: u32,
        pub qnic_interface: Interface,
    }

    impl MeasureCount {
        pub fn new() -> Self {
            MeasureCount {
                name: String::from("measure_count"),
                count: 0,
                qnic_interface: Interface::place_holder(),
            }
        }
        pub fn from(count: u32, qnic_interface: Interface) -> Self {
            MeasureCount {
                name: String::from("measure_count"),
                count: count,
                qnic_interface: qnic_interface,
            }
        }
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Fidelity {
        name: String,
        pub required_fidelity: f64,
        pub qnic_interface: Interface,
    }

    impl Fidelity {
        pub fn new() -> Self {
            Fidelity {
                name: String::from("fidelity"),
                required_fidelity: 0.0,
                qnic_interface: Interface::place_holder(),
            }
        }

        pub fn from(fidelity: f64, qnic_interface: Interface) -> Self {
            Fidelity {
                name: String::from("fidelity"),
                required_fidelity: fidelity,
                qnic_interface: qnic_interface,
            }
        }
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    pub struct Wait {
        name: String,
        pub qnic_interface: Interface,
    }

    impl Wait {
        pub fn new() -> Self {
            Wait {
                name: String::from("wait"),
                qnic_interface: Interface::place_holder(),
            }
        }

        pub fn from(qnic_interface: Interface) -> Self {
            Wait {
                name: String::from("wait"),
                qnic_interface: qnic_interface,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_condition_clause() {
        let mut condition = Condition::new(None);
        let interface = Interface::place_holder();
        // from(fidelity: f64, qnic_interface: Interface)
        let fidelity_clause =
            v1::ConditionClauses::Fidelity(v1::Fidelity::from(0.95, interface.clone()));
        condition.add_condition_clause(fidelity_clause);
        assert_eq!(condition.name, None);
        assert_eq!(condition.clauses.len(), 1);
        assert_eq!(
            condition.clauses[0],
            v1::ConditionClauses::Fidelity(v1::Fidelity::from(0.95, interface))
        );
    }
}
