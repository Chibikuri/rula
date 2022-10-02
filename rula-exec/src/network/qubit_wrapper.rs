use serde::{Deserialize, Serialize};

// Qubit interface wrapper
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct Qubit {
    // address: QubitAddress,
}

impl Qubit {
    pub fn new() -> Self {
        Qubit {}
    }
}
