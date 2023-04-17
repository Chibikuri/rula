use super::condition::*;
use super::ruleset::Rule;
use crate::ruleset_gen::ruleset::AddressKind;
use std::cell::RefCell;
use std::rc::Rc;

// Possible Type values in RuLa
#[derive(Debug, Clone, PartialEq)]
pub enum Types {
    // Composite types
    Repeater,
    Message,
    Result,
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
    RuLaResult(RuLaResult),
    RuLaVec(Vec<RuLaValue>),
}

impl RuLaValue {
    pub fn wrap<T>(val: &T) -> Self {
        match std::any::type_name::<T>() {
            // "Repeater" => {RuLaValue::Repeater(val as Repeater)}
            _ => todo!(),
        }
    }

    pub fn eval_as_result(&self) -> &RuLaResult {
        match self {
            RuLaValue::RuLaResult(result) => result,
            _ => panic!("This value cannot be evaluated as result"),
        }
    }

    pub fn eval_as_repeater(&self) -> &Repeater {
        match self {
            RuLaValue::Repeater(repeater) => repeater,
            _ => panic!("This value cannot be evaluated as repeater"),
        }
    }
    pub fn eval_as_message(&self) -> &Message {
        match self {
            RuLaValue::Message(message) => message,
            _ => panic!("This value cannot be evaluated as message"),
        }
    }
    pub fn eval_as_qubit(&self) -> &Qubit {
        match self {
            RuLaValue::Qubit(qubit) => qubit,
            _ => panic!("This value cannot be evaluated as Qubit"),
        }
    }
    pub fn eval_as_str(&self) -> &String {
        match self {
            RuLaValue::Str(string) => string,
            _ => panic!("This value cannot be evaluated as Str"),
        }
    }
    pub fn eval_as_uint(&self) -> u64 {
        match self {
            RuLaValue::UInt(uint) => uint.clone(),
            _ => panic!("This value cannot be evaluated as UInt"),
        }
    }
    pub fn eval_as_int(&self) -> i64 {
        match self {
            RuLaValue::Int(integer) => integer.clone(),
            _ => panic!("This value cannot be evaluated as Int"),
        }
    }
    pub fn eval_as_float(&self) -> f64 {
        match self {
            RuLaValue::Float(float) => float.clone(),
            _ => panic!("This value cannot be evaluated as float"),
        }
    }
    pub fn eval_as_bool(&self) -> bool {
        match self {
            RuLaValue::Boolean(boolean) => boolean.clone(),
            _ => panic!("This value cannot evaluated as bool"),
        }
    }
}

pub type RuleVec = Rc<RefCell<Vec<RefCell<Rule>>>>;
// Repeater type
#[derive(Debug, Clone, PartialEq)]
pub struct Repeater {
    // A readable name of this repeater
    pub name: String,
    // An index of this repeater.
    // This is just for internal use to identify which repeater is which.
    pub index: u64,
    // A public address exposed to outer world
    pub address: AddressKind,
    // repeater chains
    // (initiators) left_repeaters < -- > self (0) < -- > right_repeaters (responder)
    pub left_repeaters: Vec<Box<Repeater>>,
    pub right_repeaters: Vec<Box<Repeater>>,
}

impl Repeater {
    pub fn new(name: &str) -> Self {
        Repeater {
            name: name.to_string(),
            index: 0,
            address: AddressKind::IntegerKind(0),
            left_repeaters: vec![],
            right_repeaters: vec![],
        }
    }

    pub fn update_index(&mut self, new_index: u64) {
        self.index = new_index
    }

    pub fn update_address(&mut self, address: AddressKind) {
        self.address = address
    }

    // TODO: remove rulevec arg
    pub fn hop(&self, _: RuleVec, distance_ref: &i64) -> &Repeater {
        let distance = distance_ref.clone();
        if distance == 0 {
            return self;
        } else if distance > 0 {
            let index = (distance - 1) as usize;
            if index >= self.right_repeaters.len() {
                panic!(
                    "No more repeaters at responder side index: {}, num_right: {}",
                    (distance - 1),
                    self.right_repeaters.len()
                );
            }
            &*self.right_repeaters[index]
        } else {
            let index = (-distance - 1) as usize;
            if index >= self.left_repeaters.len() {
                panic!(
                    "No more repeaters at initiator side: index: {}, num_left: {}",
                    (-distance - 1),
                    self.left_repeaters.len()
                );
            }
            &*self.left_repeaters[index]
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
pub struct Message {
    pub result: RuLaResult,
    pub source: AddressKind,
}

impl Message {
    pub fn place_holder() -> Self {
        let result = RuLaResult::new();
        Message {
            result: result,
            source: AddressKind::IntegerKind(0),
        }
    }
    pub fn update_source(&mut self, partner_address: AddressKind) {
        self.source = partner_address;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Qubit {
    pub index: u64,
}

impl Qubit {
    pub fn new(index: u64) -> Self {
        Qubit { index: index }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RuLaResult {}

impl RuLaResult {
    pub fn new() -> Self {
        RuLaResult {}
    }
    pub fn comparable(&self) -> impl Fn(&str) -> CmpTarget {
        self.__cmp_target()
    }
    // TODO: accept general identifier for the previous result
    pub fn __cmp_target(&self) -> impl Fn(&str) -> CmpTarget {
        |value| CmpTarget::MeasResult(String::from(value))
    }
}
