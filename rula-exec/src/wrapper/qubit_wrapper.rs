use serde::{Deserialize, Serialize};

// Qubit interface wrapper
#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct QubitInterfaceWrapper {
    // address: QubitAddress,
}

impl QubitInterfaceWrapper {
    pub fn new() -> Self {
        QubitInterfaceWrapper {}
    }
}
