use std::collections::HashMap;
use std::net::IpAddr;
use crate::hardware::mock_qnic::MockQnic;

// This routing table is highly abstracted for RuLa test purpose
// Implementation might be different from actual implementation
pub struct MockQnicRoutingTable{
    // <Destination, QnicInterface>
    pub table: HashMap<IpAddr, MockQnic>,
    // In usual 
    pub distance: HashMap<u32, IpAddr>,
}

impl MockQnicRoutingTable{
    pub fn add_destination(&mut self, destination: IpAddr, qnic_interface: MockQnic){
        self.table.insert(destination, qnic_interface);
    }

    pub fn find_interface(&self, destination: &IpAddr)-> &MockQnic{
        if !self.table.contains_key(destination){
            panic!("No interface to the destination found");
        }
        self.table.get(destination).expect("Something wrong happened in finding qnic")
    }

    pub fn add_distance(&mut self, distance: u32, destination: IpAddr){
        self.distance.insert(distance, destination);
    }

    pub fn get_interface_by_distance(&mut self, distance: u32) -> &IpAddr{
        if !self.distance.contains_key(&distance){
            panic!("No distance found");
        }
        self.distance.get(&distance).expect("Something wrong happend in find")
    }
}

