use serde::{Deserialize, Serialize};
use std::net::{IpAddr, Ipv4Addr};

use mock_components::hardware::qnic::QnicType;

use crate::rulep::condition::v1::EnoughResource;

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct QnicInterfaceWrapper {
    // pub partner_address: IpAddr,
    pub qnic_type: QnicType,
    pub qnic_id: u32,
    pub qnic_address: Option<IpAddr>,
}

/// This should be a wrapper of actual QNIC interface
impl QnicInterfaceWrapper {
    pub fn from(qnic_type: QnicType, qnic_id: u32, qnic_address: Option<IpAddr>) -> Self {
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

    // wrapper functin to request resource to resource allocator
    pub fn request_resource(gen_ruleset: bool) -> Option<EnoughResource> {
        None
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
