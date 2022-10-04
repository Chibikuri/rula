use std::fmt::Debug;

#[derive(Debug)]
pub enum HardwareError {
    NoInstructionFound,
    NoQubitFound,
    Test,
}
