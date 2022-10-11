use std::fmt::Debug;

#[derive(Debug)]
pub enum HardwareError {
    NoInstructionFound,
    NoQubitFound,
    NoControlQubitFound,
    NoMeasurementFound,
    QubitAlreadyBusy,
    QubitNotBusy,
    InvalidBasis,
    Test,
}
