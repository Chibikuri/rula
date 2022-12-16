// use super::result::{MeasResult, QResult};
// use super::RuleVec;
// use serde::{Deserialize, Serialize};
// use std::net::IpAddr;

// #[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
// pub struct Message {
//     pub kind: String,
//     pub src: IpAddr,
//     pub dst: IpAddr,
//     pub body: QResult,
// }

// impl Message {
//     pub fn new(kind: &str, src: &IpAddr, dst: &IpAddr, result: QResult) -> Message {
//         Message {
//             kind: String::from(kind),
//             src: src.clone(),
//             dst: dst.clone(),
//             body: result,
//         }
//     }
//     pub async fn append_body(&mut self, result: &QResult) {}
//     pub fn __static__append_body(&mut self, _: RuleVec, result: QResult) {}
// }

// pub fn Message(kind: &str, src_addr: &IpAddr, dst_addr: &IpAddr) -> Message {
//     Message::new(
//         kind,
//         src_addr,
//         dst_addr,
//         QResult {
//             result: MeasResult {
//                 qubit_address: 0,
//                 output: "00".to_string(),
//             },
//             generated_actions: vec![],
//         },
//     )
// }

// pub fn __static__Message(
//     _: RuleVec,
//     kind: String,
//     src_qnic_addr: IpAddr,
//     dst_qnic_addr: IpAddr,
// ) -> Message {
//     Message::new(
//         &kind,
//         &src_qnic_addr,
//         &dst_qnic_addr,
//         QResult {
//             result: MeasResult {
//                 qubit_address: 0,
//                 output: "00".to_string(),
//             },
//             generated_actions: vec![],
//         },
//     )
// }
