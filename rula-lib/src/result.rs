use super::ruleset::action::v2::ActionClauses;
use super::ruleset::condition::v1::{CmpKind, CmpTarget};
use super::RuleVec;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct QResult {
    pub result: MeasResult,
    pub generated_actions: Vec<ActionClauses>,
}

impl QResult {
    pub async fn add_tag(&mut self, tag: &str) -> &mut Self {
        self
    }
    pub fn __static__add_tag(&mut self, _: RuleVec, tag: String) -> &mut Self {
        self
    }
    pub async fn add_result(&mut self, result: &String) {}
    pub fn __static__add_result(&mut self, _: RuleVec, result: String) {}

    pub async fn get_output(&self) -> String {
        self.result.get_output().await
    }
    pub fn __static__get_output(&self, _: RuleVec) -> String {
        String::from("__static__result")
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct MeasResult {
    pub qubit_address: u32,
    pub output: String,
}

impl MeasResult {
    pub async fn get_output(&self) -> String {
        self.output.clone()
    }
    pub fn __static__get_output(&self, _: RuleVec) -> String {
        // Nothing happen in the static ruleset generation
        String::from("__static__result")
    }

    pub fn get_result(&self) -> &str {
        &self.output
    }

    pub fn __static__get_result(&self, _: RuleVec) -> (&str, CmpKind, impl Fn(&str) -> CmpTarget) {
        ("__static__result", self.__cmp_kind(), self.__cmp_target())
    }
    pub fn __cmp_kind(&self) -> CmpKind {
        CmpKind::MeasResult
    }

    pub fn __cmp_target(&self) -> impl Fn(&str) -> CmpTarget {
        |value| CmpTarget::MeasResult(String::from(value))
    }
}

use super::qubit::QubitInterface;
pub fn Result(qubit: &QubitInterface) -> QResult {
    QResult {
        result: MeasResult {
            qubit_address: 0,
            output: "00".to_string(),
        },
        generated_actions: vec![],
    }
}

pub fn __static__Result(_: RuleVec, qubit: QubitInterface) -> QResult {
    QResult {
        result: MeasResult {
            qubit_address: 0,
            output: "00".to_string(),
        },
        generated_actions: vec![],
    }
}
