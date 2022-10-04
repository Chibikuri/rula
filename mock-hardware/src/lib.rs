pub mod hardware {
    pub mod bsa;
    pub mod error;
    pub mod qnic;
    pub mod qubit;
    pub mod result;
    use error::HardwareError;
    pub type IResult<T> = std::result::Result<T, HardwareError>;
}
