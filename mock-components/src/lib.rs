pub mod hardware {
    pub mod error;
    pub mod qnic;
    pub mod qubit;
    pub mod result;
    use error::HardwareError;
    pub type IResult<T> = std::result::Result<T, HardwareError>;
}

pub mod software{
    pub mod resource_allocator;
}
