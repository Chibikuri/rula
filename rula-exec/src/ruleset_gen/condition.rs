use super::ruleset::{PartnerAddr, ProtocolMessages};
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
    pub fn update_condition_name(&mut self, new_name: &str) {
        self.name = Some(String::from(new_name));
    }
}

// Awaitable conditions that can be met in the future
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum ConditionClauses {
    /// Variable comparison
    Cmp(Cmp),
    /// Timer expiration
    Timer(f64),
    /// The number of available resources in the QNIC
    Res(Res),
    /// Received the message
    Recv(Recv),
    /// Wait until the specificmessage arrives
    Wait(ProtocolMessages),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Cmp {
    pub cmp_val: CmpKind,
    pub operator: CmpOp,
    pub target_val: CmpTarget,
}

impl Cmp {
    pub fn new(op: CmpOp, target_val: CmpTarget) -> Self {
        let cmp_val = target_val.kind();
        Cmp {
            cmp_val: cmp_val,
            operator: op,
            target_val: target_val,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum CmpTarget {
    MeasResult(String),
    Fidelity(f64),
    MeasCount(u64),
}

impl CmpTarget {
    pub fn kind(&self) -> CmpKind {
        match self {
            CmpTarget::MeasResult(_) => CmpKind::MeasResult,
            CmpTarget::Fidelity(_) => CmpKind::Fidelity,
            CmpTarget::MeasCount(_) => CmpKind::MeasCount,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum MeasResult {
    // concrete value
    Value(String),
    // identifier for previous result
    Id(String),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum CmpKind {
    MeasResult,
    Fidelity,
    MeasCount,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum CmpOp {
    Lt,  // <
    Leq, // <=
    Gt,  // >
    Geq, // >=
    Eq,  // ==
    Neq, // !=
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Res {
    name: String,
    pub num_required_resource: u64,
    pub partner_addr: PartnerAddr,
    pub required_fidelity: Option<f64>,
}

impl Res {
    pub fn new(
        num_resource: u64,
        partner_addr: PartnerAddr,
        required_fidelity: Option<f64>,
    ) -> Self {
        Res {
            name: String::from("enough_resource"),
            num_required_resource: num_resource,
            partner_addr: partner_addr,
            required_fidelity: required_fidelity,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Recv {
    pub partner_index: u64,
}

impl Recv {
    pub fn new(partner_index: u64) -> Self {
        Recv {
            partner_index: partner_index,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Wait {
    name: String,
}

impl Wait {
    pub fn new() -> Self {
        Wait {
            name: String::from("wait"),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::net::{IpAddr, Ipv4Addr};

    use crate::ruleset_gen::ruleset::ProtoMessageIdentifier;

    use super::*;

    #[test]
    fn test_condition_clause() {
        let mut condition = Condition::new(None);
        // from(fidelity: f64, qnic_interface: Interface)
        let fidelity_clause = ConditionClauses::Wait(ProtocolMessages::Free(
            ProtoMessageIdentifier::new(PartnerAddr::IntegerKind(1)),
        ));
        condition.add_condition_clause(fidelity_clause);
        assert_eq!(condition.name, None);
        assert_eq!(condition.clauses.len(), 1);
        assert_eq!(
            condition.clauses[0],
            ConditionClauses::Wait(ProtocolMessages::Free(ProtoMessageIdentifier::new(
                PartnerAddr::IntegerKind(1)
            )))
        );
    }

    #[test]
    fn test_cmp_clause() {
        let cmp_clause = Cmp::new(CmpOp::Eq, CmpTarget::MeasResult(String::from("00")));
        assert_eq!(cmp_clause.cmp_val, CmpKind::MeasResult);
        assert_eq!(cmp_clause.operator, CmpOp::Eq);
        assert_eq!(
            cmp_clause.target_val,
            CmpTarget::MeasResult(String::from("00"))
        );
    }

    #[test]
    fn test_enough_resource_clause() {
        let enough_resource = Res::new(
            2,
            PartnerAddr::IpKind(IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0))),
            Some(0.8),
        );
        assert_eq!(enough_resource.num_required_resource, 2);
        assert_eq!(enough_resource.required_fidelity, Some(0.8));
    }
}
