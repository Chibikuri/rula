use crate::hardware::mock_qnic::MockQnic;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::net::IpAddr;
use std::net::Ipv4Addr;

// This routing table is highly abstracted for RuLa test purpose
// Implementation might be different from actual implementation
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct MockQnicRoutingTable {
    // <Destination, QnicInterface>
    pub table: HashMap<IpAddr, MockQnic>,
    // In usual
    pub distance: HashMap<u64, IpAddr>,
}

impl MockQnicRoutingTable {
    pub fn new() -> Self {
        MockQnicRoutingTable {
            table: HashMap::new(),
            distance: HashMap::new(),
        }
    }
    pub fn add_destination(&mut self, destination: IpAddr, qnic_interface: MockQnic) {
        self.table.insert(destination, qnic_interface);
    }

    pub fn find_interface(&self, destination: &IpAddr) -> &MockQnic {
        if !self.table.contains_key(destination) {
            panic!("No interface to the destination found");
        }
        self.table
            .get(destination)
            .expect("Something wrong happened in finding qnic")
    }

    pub fn add_distance(&mut self, distance: u64, destination: IpAddr) {
        self.distance.insert(distance, destination);
    }

    pub fn get_interface_by_distance(&self, distance: u64) -> &IpAddr {
        if !self.distance.contains_key(&distance) {
            panic!("No distance found");
        }
        self.distance
            .get(&distance)
            .expect("Something wrong happend in find")
    }

    // For debugging purpose
    // Brush up this later for testing
    pub fn generate_mock_table(&mut self) {
        let qnic1 = MockQnic::new();
        let qnic2 = MockQnic::new();
        self.add_destination(IpAddr::V4(Ipv4Addr::new(192, 168, 0, 1)), qnic1);
        self.add_destination(IpAddr::V4(Ipv4Addr::new(192, 168, 0, 2)), qnic2);

        // Single hop
        self.add_distance(1, IpAddr::V4(Ipv4Addr::new(192, 168, 0, 1)));
        self.add_distance(1, IpAddr::V4(Ipv4Addr::new(192, 168, 0, 2)));
        // Two hop
        self.add_distance(2, IpAddr::V4(Ipv4Addr::new(192, 168, 0, 3)));
        self.add_distance(2, IpAddr::V4(Ipv4Addr::new(192, 168, 0, 4)));
        // Three hop
        self.add_distance(3, IpAddr::V4(Ipv4Addr::new(192, 168, 0, 5)));
        self.add_distance(3, IpAddr::V4(Ipv4Addr::new(192, 168, 0, 6)));
    }
}
