pub mod hardware {
    pub mod bsa;
    pub mod qnic;
    pub mod qubit;
    pub mod error;
    use error::HardwareError;
    pub type IResult<T> = std::result::Result<T, HardwareError>;
}
