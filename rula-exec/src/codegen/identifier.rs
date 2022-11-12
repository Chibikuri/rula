use rula_parser::parser::ast::IdentType;
use std::collections::HashMap;

// This should be fixed in the future
// If there are two same name variables in the different rules, this would duplicate it.
#[derive(Debug, Clone, PartialEq)]
pub struct IdentTracker {
    pub identifiers: HashMap<String, Identifier>,
    // Should have better way to track this
    pub config_name: Option<String>,
}

impl IdentTracker {
    pub fn new() -> Self {
        IdentTracker {
            identifiers: HashMap::new(),
            config_name: None,
        }
    }
    pub fn register(&mut self, ident_name: &str, ident: Identifier) {
        self.identifiers.insert(ident_name.to_string(), ident);
    }

    pub fn update_config_name(&mut self, config_name: &str) {
        self.config_name = Some(String::from(config_name));
    }

    pub fn check_ident_type(&mut self, ident_name: &str) -> IdentType {
        match self.identifiers.get(ident_name) {
            Some(ident) => ident.ident_type.clone(),
            None => IdentType::Other,
        }
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
    Qubit,
    Vector,
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
