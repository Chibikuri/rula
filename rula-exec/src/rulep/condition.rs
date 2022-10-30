use crate::network::qnic_wrapper::QnicInterfaceWrapper;
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
    pub fn update_condition_name(&mut self, new_name: &str) {
        self.name = Some(String::from(new_name));
    }
}

pub mod v1 {
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
        pub required_fidelity: Option<f64>,
        pub qnic_interface: Option<QnicInterfaceWrapper>,
    }

    impl EnoughResource {
        pub fn new(
            num_resource: u32,
            required_fidelity: Option<f64>,
            qnic_interface: Option<QnicInterfaceWrapper>,
        ) -> Self {
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
        pub qnic_interface: Option<QnicInterfaceWrapper>,
    }

    impl MeasureCount {
        pub fn new(count: u32, qnic_interface: Option<QnicInterfaceWrapper>) -> Self {
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
        pub qnic_interface: Option<QnicInterfaceWrapper>,
    }

    impl Fidelity {
        pub fn new(fidelity: f64, qnic_interface: Option<QnicInterfaceWrapper>) -> Self {
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
        pub qnic_interface: Option<QnicInterfaceWrapper>,
    }

    impl Wait {
        pub fn new(qnic_interface: Option<QnicInterfaceWrapper>) -> Self {
            Wait {
                name: String::from("wait"),
                qnic_interface: qnic_interface,
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_condition_clause() {
            let mut condition = Condition::new(None);
            let interface = QnicInterfaceWrapper::place_holder();
            // from(fidelity: f64, qnic_interface: Interface)
            let fidelity_clause = v1::ConditionClauses::Fidelity(v1::Fidelity::new(0.95, None));
            condition.add_condition_clause(fidelity_clause);
            assert_eq!(condition.name, None);
            assert_eq!(condition.clauses.len(), 1);
            assert_eq!(
                condition.clauses[0],
                v1::ConditionClauses::Fidelity(v1::Fidelity::new(0.95, None))
            );
        }

        #[test]
        fn test_enough_resource_clause() {
            let enough_resource = EnoughResource::new(2, Some(0.8), None);
            assert_eq!(enough_resource.num_required_resource, 2);
            assert_eq!(enough_resource.required_fidelity, Some(0.8));
            assert_eq!(enough_resource.qnic_interface, None);
        }

        #[test]
        fn test_measure_count_clause() {
            let measure_count = MeasureCount::new(8000, None);
            assert_eq!(measure_count.count, 8000);
            assert_eq!(measure_count.qnic_interface, None);
        }

        #[test]
        fn test_fidelity_clause() {
            let fidelity = Fidelity::new(0.8, None);
            assert_eq!(fidelity.required_fidelity, 0.8);
            assert_eq!(fidelity.qnic_interface, None);
        }

        #[test]
        fn test_wait_clause() {
            let wait = Wait::new(None);
            assert_eq!(wait.qnic_interface, None);
        }
    }
}
