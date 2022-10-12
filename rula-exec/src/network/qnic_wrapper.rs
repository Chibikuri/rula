use serde::{Deserialize, Serialize};
use std::net::{IpAddr, Ipv4Addr};

use mock_components::hardware::qnic::QnicType;

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Interface {
    // pub partner_address: IpAddr,
    pub qnic_type: QnicType,
    pub qnic_id: u32,
    pub qnic_address: IpAddr,
}

/// This should be a wrapper of actual QNIC interface
impl Interface {
    pub fn from(qnic_type: QnicType, qnic_id: u32, qnic_address: IpAddr) -> Self {
        Interface {
            qnic_type: qnic_type,
            qnic_id: qnic_id,
            qnic_address: qnic_address,
        }
    }

    pub fn place_holder() -> Self {
        Interface {
            // partner_address: IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)),
            qnic_type: QnicType::QnicN,
            qnic_id: 0,
            qnic_address: IpAddr::V4(Ipv4Addr::new(0, 0, 0, 0)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum InstructionSet {}
