use rula_parser::parser::ast::IdentType;
use std::collections::HashMap;

// This should be fixed in the future
// If there are two same name variables in the different rules, this would duplicate it.
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
    pub fn check_ident_type(&mut self, ident_name: &str) -> IdentType {
        match self.identifiers.get(ident_name) {
            Some(ident) => ident.ident_type.clone(),
            None => IdentType::Other,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub ident_type: IdentType,
    pub type_hint: TypeHint,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeHint {
    Str,
    Integer64,
    UnsignedInteger64,
    Float64,
    Boolean,
    Unknown,
}

impl Identifier {
    pub fn new(ident_type: IdentType, type_hint: TypeHint) -> Self {
        Identifier {
            ident_type: ident_type,
            type_hint: type_hint,
        }
    }
}
