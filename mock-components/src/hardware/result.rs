use serde::{Deserialize, Serialize};
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct MeasResult {
    pub basis: MeasBasis,
    pub result: Outcome,
}

impl MeasResult {
    pub fn new(basis: MeasBasis, result: Outcome) -> Self {
        MeasResult {
            basis: basis,
            result: result,
        }
    }
    pub fn place_holder() -> Self {
        MeasResult {
            basis: MeasBasis::PlaceHolder,
            result: Outcome::PlaceHolder,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum MeasBasis {
    X,
    Y,
    Z,
    PlaceHolder,
}
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum Outcome {
    Zero,
    One,
    PlaceHolder,
}
