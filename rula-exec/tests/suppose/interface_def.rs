use once_cell::sync::OnceCell;
use std::sync::Mutex;
use std::collections::HashMap;
use std::net::IpAddr;

static INTERFACE: OnceCell<Mutex<InterfaceGroup>> = OnceCell::new();

#[derive(Debug)]
pub struct InterfaceGroup{
    pub interfaces: HashMap<String, QNicInterface>,
}

impl InterfaceGroup{
    pub fn new() -> Self{
        InterfaceGroup{interfaces: HashMap::new()}
    }
    pub fn add_interface(&mut self, name: &str, interface: QNicInterface){
        self.interfaces.insert(name.to_string(), interface);
    }
}

#[derive(Debug)]
pub struct QNicInterface{
    pub message_box: HashMap<RuleIdentifier, Message>,
    pub qubits: HashMap<u64, QubitInterface>,
}

impl QNicInterface{
    pub fn new() -> QNicInterface{
        QNicInterface{
            message_box: HashMap::new(),
            qubits: HashMap::new()   
        }
    }
}

#[derive(Debug)]
pub struct Message{
    
}

#[derive(Debug)]
pub struct RuleIdentifier{
    pub qnic_address: IpAddr, 
    pub rule_id: u32,
}


#[derive(Debug)]
pub struct QubitInterface{
    
}



fn main(){
    // #interface: {qn0, qn1} => qnall
    // rule entanglement_swapping <qn0, qn1>(){}
    let interface = qnicinterfaces.get_or_init(|| Mutex::new(InterfaceGroup::new()));
    interface.lock().unwrap().add_interface("qn0", QNicInterface::new());
    println!("{:#?}", interface);
    // take 

}