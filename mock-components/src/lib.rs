pub mod hardware {
    pub mod error;
    pub mod mock_qnic;
    pub mod mock_qubit;
    pub mod result;
    use error::HardwareError;
    pub type IResult<T> = std::result::Result<T, HardwareError>;
}

pub mod software {
    pub mod resource_allocator;
    pub mod mock_routing_daemon;
}
