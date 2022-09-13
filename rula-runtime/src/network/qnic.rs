// Wrapper for QNIC interface
//
// extern crate pnet;
// extern crate pnet_datalink;

// use pnet_datalink::{self, NetworkInterface};
// // use pnet::datalink::Channel::Ethernet;
// use pnet::packet::{Packet, MutablePacket};
// use pnet::packet::ethernet::{EthernetPacket, MutableEthernetPacket};

// use std::env;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum QnicType {
    QnicE,
    QnicP,
    QnicRp,
    QnicN, // place holder
}
