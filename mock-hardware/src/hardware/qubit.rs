pub struct MockQubit {
    pub address: u64,
}

impl MockQubit {
    pub fn new(address: u64) -> Self {
        MockQubit { address: address }
    }
}
