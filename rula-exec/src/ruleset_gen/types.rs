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
pub enum RuLaValue {
    Repeater(Repeater),
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

impl RuLaValue {
    pub fn eval_as_repeater(&self) -> &Repeater {
        match self {
            RuLaValue::Repeater(repeater) => repeater,
            _ => panic!("This value cannot evaluated as repeater"),
        }
    }
    pub fn eval_as_message(&self) -> &Message {
        match self {
            RuLaValue::Message(message) => message,
            _ => panic!("This value cannot evaluated as message"),
        }
    }
    pub fn eval_as_qubit(&self) -> &Qubit {
        match self {
            RuLaValue::Qubit(qubit) => qubit,
            _ => panic!("This value cannot evaluated as Qubit"),
        }
    }
    pub fn eval_as_str(&self) -> &String {
        match self {
            RuLaValue::Str(string) => string,
            _ => panic!("This value cannot evaluated as Str"),
        }
    }
    pub fn eval_as_uint(&self) -> u64 {
        match self {
            RuLaValue::UInt(uint) => uint.clone(),
            _ => panic!("This value cannot evaluated as UInt"),
        }
    }
    pub fn eval_as_int(&self) -> i64 {
        match self {
            RuLaValue::Int(integer) => integer.clone(),
            _ => panic!("This value cannot evaluated as Int"),
        }
    }
    pub fn eval_as_float(&self) -> f64 {
        match self {
            RuLaValue::Float(float) => float.clone(),
            _ => panic!("This value cannot evaluated as float"),
        }
    }
    pub fn eval_as_bool(&self) -> bool {
        match self {
            RuLaValue::Boolean(boolean) => boolean.clone(),
            _ => panic!("This value cannot evaluated as bool"),
        }
    }
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
    pub fn hop(&self, distance: i64) -> &Repeater {
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
