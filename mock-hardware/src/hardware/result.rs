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
}

#[derive(Debug)]
pub enum MeasBasis {
    X,
    Y,
    Z,
}
#[derive(Debug)]
pub enum Outcome {
    Zero,
    One,
}
