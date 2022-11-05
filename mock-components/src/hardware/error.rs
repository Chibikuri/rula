use std::fmt::Debug;

#[derive(Debug)]
pub enum HardwareError {
    NoInstructionFound,
    NoQubitFound,
    NoControlQubitFound,
    NoMeasurementFound,
    QubitAlreadyInUse,
    QubitAlreadyBusy,
    QubitNotBusy,
    InvalidBasis,
    Test,
}
