use std::fmt;

#[derive(Debug, PartialEq)]
pub enum RuLaError {
    // Should be detailed
    RuLaSyntaxError,
    RuLaPathConstructionError,
    InterfaceDuplicationError,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RuLaSyntaxError;

#[derive(Debug, Clone, PartialEq)]
pub struct RuLaPathConstructionError;

impl fmt::Display for RuLaSyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Syntax Error")
    }
}
