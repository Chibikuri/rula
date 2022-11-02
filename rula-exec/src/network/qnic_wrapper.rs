extern crate rula_derive;

use proc_macro2::TokenStream;
use quote::quote;
use serde::{Deserialize, Serialize};
use std::error::Error;
use std::net::{IpAddr, Ipv4Addr};

use crate::rulep::condition::v1::*;
use mock_components::hardware::qnic::QnicType;

type IResult<T> = Result<T, QnicInterfaceWrapperError>;

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum QnicInterfaceWrapperError {}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct QnicInterfaceWrapper {
    // pub partner_address: IpAddr,
    pub qnic_type: QnicType,
    pub qnic_id: u32,
    pub qnic_address: Option<IpAddr>,
}

/// This should be a wrapper of actual QNIC interface
impl QnicInterfaceWrapper {
    pub fn new(qnic_type: QnicType, qnic_id: u32, qnic_address: Option<IpAddr>) -> Self {
        QnicInterfaceWrapper {
            qnic_type: qnic_type,
            qnic_id: qnic_id,
            qnic_address: qnic_address,
        }
    }

    pub fn place_holder() -> Self {
        QnicInterfaceWrapper {
            // partner_address: IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)),
            qnic_type: QnicType::QnicN,
            qnic_id: 0,
            qnic_address: None,
        }
    }

    // pub fn gen_ruleset_instruction()
    // This function supposed to be an builtin function for quantum interface
    // wrapper functin to request resource to resource allocator
    pub fn request_resource(&self, _gen_ruleset: bool) -> IResult<ConditionClauses> {
        Ok(ConditionClauses::EnoughResource(EnoughResource::new(
            1, None, None,
        )))
    }

    pub fn get_qubit_by_partner() {}
    pub fn get_message(&self) -> IResult<ConditionClauses> {
        // This needs to be updated
        Ok(ConditionClauses::Wait)
    }

    pub fn get_partner_by_hop(&self, _distance: u32) {
        // If the distance is 0, that means
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum InstructionSet {}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_interface_place_holder() {
        let interface = QnicInterfaceWrapper::place_holder();
        assert_eq!(interface.qnic_type, QnicType::QnicN);
        assert_eq!(interface.qnic_id, 0);
        assert_eq!(interface.qnic_address, None);
    }
}
