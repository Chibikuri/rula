use std::error;
use std::error::Error as _;
use std::fmt;

// Custome error interface for rula
pub type IResult<T> = std::result::Result<T, RuLaSyntaxError>;

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
