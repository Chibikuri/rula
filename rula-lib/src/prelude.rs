use super::qubit::QubitInterface;
use super::*;
use crate::ruleset::action::v2::Send;
use crate::ruleset::ruleset::InterfaceInfo;
use message::Message;

pub async fn free(qubit: &QubitInterface) {}
pub fn __static__free(rules: RuleVec, _: QubitInterface) {
    for rule in &*rules.borrow_mut() {
        rule.borrow_mut().add_action_clause(ActionClauses::Free);
    }
}

pub async fn promote(qubit: &QubitInterface) {}

pub fn __static__promote(rules: RuleVec, _: QubitInterface) {
    for rule in &*rules.borrow_mut() {
        rule.borrow_mut().add_action_clause(ActionClauses::Promote)
    }
}
pub async fn send(message: &Message) {}
pub fn __static__send(rules: RuleVec, message: Message) {
    for rule in &*rules.borrow_mut() {
        rule.borrow_mut()
            .add_action_clause(ActionClauses::Send(Send::new(message.src, message.dst)));
    }
}
pub fn __get_interface_info(name: &String) -> InterfaceInfo {
    InterfaceInfo::new(None, None, None, None)
}

pub fn __comp<T: PartialOrd>(lhs: T, op: __CmpOp, rhs: T) -> bool {
    op.cmp(lhs, rhs)
}

pub fn __static__comp<T: PartialOrd>(lhs: T, op: __CmpOp, rhs: T, rules: RuleVec) -> bool {
    op.cmp(lhs, rhs)
}

#[derive(Debug, Clone, PartialEq)]
pub enum __CmpOp {
    Lt,  // <
    Gt,  // >
    LtE, // <=
    GtE, // >=
    Eq,  // ==
    Nq,  // !=
}

impl __CmpOp {
    pub fn cmp<T: PartialOrd>(&self, lhs: T, rhs: T) -> bool {
        match self {
            __CmpOp::Lt => lhs < rhs,
            __CmpOp::Gt => lhs > rhs,
            __CmpOp::LtE => lhs <= rhs,
            __CmpOp::GtE => lhs >= rhs,
            __CmpOp::Eq => lhs == rhs,
            __CmpOp::Nq => lhs != rhs,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub filled: bool,
    pub value: ArgVal,
    pub type_hint: LibTypeHint,
}

// TODO: Argument func should be more flex
impl Argument {
    pub fn init() -> Self {
        // FIXME: Initializing "" would not be a good idea
        Argument {
            filled: false,
            value: ArgVal::PlaceHolder,
            type_hint: LibTypeHint::Unknown,
        }
    }

    pub fn eval_str(&self) -> String {
        self.value.eval_str()
    }

    pub fn eval_float64(&self) -> f64 {
        self.value.eval_float64()
    }

    pub fn eval_int64(&self) -> i64 {
        self.value.eval_int64()
    }

    pub fn eval_bool(&self) -> bool {
        self.value.eval_bool()
    }

    pub fn eval_unsigned_int64(&self) -> u64 {
        self.value.eval_unsigned_int64()
    }

    pub fn add_argument(&mut self, value: ArgVal, type_hint: LibTypeHint) {
        self.filled = true;
        self.value = value;
        self.type_hint = type_hint;
    }

    pub fn resolved(&self) -> bool {
        self.filled
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArgVal {
    Str(String),
    Integer64(i64),
    Float64(f64),
    UnsignedInteger64(u64),
    Boolean(bool),
    StrVector(Vec<String>),
    I64Vector(Vec<i64>),
    F64Vector(Vec<f64>),
    U64Vector(Vec<u64>),
    BoolVector(Vec<bool>),
    PlaceHolder,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LibTypeHint {
    Str,
    Inteter64,
    UnsignedInteger64,
    Float64,
    Boolean,
    Vector(Box<LibTypeHint>),
    Unknown,
}

impl ArgVal {
    pub fn eval_str(&self) -> String {
        match self {
            ArgVal::Str(string) => string.to_string(),
            _ => {
                panic!("This needs to be string")
            }
        }
    }
    pub fn eval_int64(&self) -> i64 {
        match self {
            ArgVal::Integer64(int64) => int64.clone(),
            _ => {
                panic!("This needs to be integer 64")
            }
        }
    }
    pub fn eval_unsigned_int64(&self) -> u64 {
        match self {
            ArgVal::UnsignedInteger64(unsigned64) => unsigned64.clone(),
            _ => panic!("This needs to be unsigned integer 64"),
        }
    }
    pub fn eval_float64(&self) -> f64 {
        match self {
            ArgVal::Float64(float64) => float64.clone(),
            _ => panic!("This needs to be float 64"),
        }
    }

    pub fn eval_bool(&self) -> bool {
        match self {
            ArgVal::Boolean(boolean) => boolean.clone(),
            _ => panic!("This needs to be a boolean"),
        }
    }

    pub fn eval_str_vec(&self) -> Vec<String> {
        match self {
            ArgVal::StrVector(str_vec) => str_vec.to_vec(),
            _ => panic!("This needs to be a vector"),
        }
    }

    pub fn eval_i64_vec(&self) -> Vec<i64> {
        match self {
            ArgVal::I64Vector(i64vec) => i64vec.to_vec(),
            _ => panic!("This needs to be integer 64 vector"),
        }
    }
    pub fn eval_f64_vec(&self) -> Vec<f64> {
        match self {
            ArgVal::F64Vector(f64vec) => f64vec.to_vec(),
            _ => panic!("This needs to be float 64 vector"),
        }
    }

    pub fn eval_u64vec(&self) -> Vec<u64> {
        match self {
            ArgVal::U64Vector(u64vec) => u64vec.to_vec(),
            _ => panic!("This needs to be unsigned integer 64 vector"),
        }
    }

    pub fn eval_bool_vec(&self) -> Vec<bool> {
        match self {
            ArgVal::BoolVector(bool_vec) => bool_vec.to_vec(),
            _ => panic!("This needs to be integer 64 vector"),
        }
    }
}
