// Possible Type values in RuLa
#[derive(Debug, Clone, PartialEq)]
pub enum Types {
    // Composite types
    Repeater,
    Message,
    Qubit,
    // Primitive Types
    Str,
    UInt,
    Int,
    Float,
    Boolean,
    Vec(Box<Types>),
    Unknown,
}

impl Types {
    pub fn promotable(&self) -> bool {
        match self {
            Types::Repeater => false,
            _ => true,
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum ArgVals {
    // Repeater cannot be taken as ordinary argument
    Message(Message),
    Qubit(Qubit),
    Str(String),
    UInt(u64),
    Int(i64),
    Float(f64),
    Boolean(bool),
    // TODO
    // Vec(Box<Types>),
}

// Repeater type
#[derive(Debug, Clone, PartialEq)]
pub struct Repeater {
    pub name: String,
    // repeater chains
    // (initiators) left_repeaters < -- > self (0) < -- > right_repeaters (responder)
    pub left_repeaters: Vec<Box<Repeater>>,
    pub right_repeaters: Vec<Box<Repeater>>,
}

impl Repeater {
    pub fn new(name: &str) -> Self {
        Repeater {
            name: name.to_string(),
            left_repeaters: vec![],
            right_repeaters: vec![],
        }
    }
    pub fn hop(&self, distance: i32) -> &Repeater {
        if distance == 0 {
            return self;
        } else if distance > 0 {
            if distance as usize > self.right_repeaters.len() {
                panic!("No more repeaters at responder side");
            }
            &*self.right_repeaters[(distance - 1) as usize]
        } else {
            if distance as usize > self.left_repeaters.len() {
                panic!("No more repeaters at initiator side");
            }
            &*self.left_repeaters[(-distance - 1) as usize]
        }
    }

    pub fn add_left_repeater(&mut self, rep: &Repeater) {
        self.left_repeaters.push(Box::new(rep.clone()));
    }

    pub fn add_right_repeater(&mut self, rep: &Repeater) {
        self.right_repeaters.push(Box::new(rep.clone()));
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Message {}

#[derive(Debug, Clone, PartialEq)]
pub struct Qubit {}
