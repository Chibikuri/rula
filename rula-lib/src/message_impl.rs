
use serde::Serialize;
use std::net::IpAddr;
use super::result_impl::QResult;
use super::RuleVec;

#[derive(Serialize, Clone, Debug, PartialEq)]
pub struct Message {
    pub kind: String,
    pub src: IpAddr,
    pub dst: IpAddr,
    pub body: QResult,
}

impl Message {
    pub fn new(kind: &str, src: &IpAddr, dst: &IpAddr, result: QResult) -> Message {
        Message {
            kind: String::from(kind),
            src: src.clone(),
            dst: dst.clone(),
            body: result,
        }
    }
    pub fn append_body(&mut self, result: &QResult) {}
    pub fn __static__append_body(&mut self, _: RuleVec, result: QResult) {}
}