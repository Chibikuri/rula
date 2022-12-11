// use rula_parser::parser::ast::IdentType;
use std::collections::{HashMap, HashSet};

// // This should be fixed in the future
// // If there are two same name variables in the different rules, this would duplicate it.
#[derive(Debug, Clone, PartialEq)]
pub struct IdentTracker {
    pub identifiers: HashMap<String, Identifier>,
}

impl IdentTracker {
    pub fn new() -> Self {
        IdentTracker {
            identifiers: HashMap::new(),
        }
    }
    pub fn register(&mut self, ident_name: &str, ident: Identifier) {
        self.identifiers.insert(ident_name.to_string(), ident);
    }

    pub fn check_type_hint(&mut self, ident_name: &str) -> TypeHint {
        match self.identifiers.get(ident_name) {
            Some(ident) => ident.type_hint.clone(),
            None => TypeHint::Unknown,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub type_hint: TypeHint,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeHint {
    Str,
    Integer64,
    UnsignedInteger64,
    Float64,
    Boolean,
    Qubit,
    StrVector,
    I64Vector,
    U64Vector,
    F64Vector,
    BoolVector,
    Unknown,
}

impl Identifier {
    pub fn new(type_hint: TypeHint) -> Self {
        Identifier {
            type_hint: type_hint,
        }
    }
}
