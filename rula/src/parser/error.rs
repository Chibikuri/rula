use std::fmt;

// #[derive(Debug)]
// pub enum RuLaError{
//     // Should be detailed
//     RuLaSyntaxError,
// }

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuLaSyntaxError;

impl fmt::Display for RuLaSyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Syntax Error")
    }
}
