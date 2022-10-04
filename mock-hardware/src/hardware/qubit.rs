use super::result::{MeasBasis, MeasResult, Outcome};
use super::IResult;
use tokio::time::{sleep, Duration};

pub enum QubitInstruction {
    SetFree,
    SetBusy,
    CheckStatus,
    Gate(GateType),
    Measure(MeasBasis),
    Test,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MockQubit {
    pub address: u64,
    busy: bool,
}

impl MockQubit {
    pub fn new(address: u64) -> Self {
        MockQubit {
            address: address,
            busy: false,
        }
    }

    async fn gate(&self, gate: &GateType) {
        match gate {
            GateType::X => {
                sleep(Duration::from_nanos(10)).await;
            }
            GateType::Y => {
                sleep(Duration::from_nanos(10)).await;
            }
            GateType::Z => {
                sleep(Duration::from_nanos(10)).await;
            }
            GateType::H => {
                sleep(Duration::from_nanos(50)).await;
            }
            GateType::S => {
                sleep(Duration::from_nanos(50)).await;
            }
            GateType::T => {
                sleep(Duration::from_nanos(50)).await;
            }
            GateType::CxControl => {
                sleep(Duration::from_nanos(100)).await;
            }
            GateType::CxTarget => {
                sleep(Duration::from_nanos(100)).await;
            }
            GateType::CzControl => {
                sleep(Duration::from_nanos(100)).await;
            }
            GateType::CzTarget => {
                sleep(Duration::from_nanos(100)).await;
            }
        }
    }

    fn free(&mut self) {
        self.busy = false;
    }

    fn busy(&mut self) {
        self.busy = true;
    }

    fn check_status(&mut self) -> bool {
        self.busy
    }

    async fn measure(&mut self, meas_basis: &MeasBasis) -> IResult<Returnable> {
        sleep(Duration::from_nanos(200)).await;
        Ok(Returnable::Result(MeasResult::new(
            MeasBasis::X,
            Outcome::One,
        )))
    }

    pub async fn call_instruction(&mut self, instruction: QubitInstruction) -> IResult<Returnable> {
        match instruction {
            QubitInstruction::SetFree => {
                self.free();
                Ok(Returnable::None)
            }
            QubitInstruction::SetBusy => {
                self.busy();
                Ok(Returnable::None)
            }
            QubitInstruction::CheckStatus => {
                self.check_status();
                Ok(Returnable::None)
            }
            QubitInstruction::Gate(gate_type) => {
                MockQubit::gate(self, &gate_type).await;
                Ok(Returnable::None)
            }
            QubitInstruction::Measure(meas_basis) => {
                let result = MockQubit::measure(self, &meas_basis).await.unwrap();
                Ok(result)
            }
            _ => todo!("unknown instruction"),
        }
    }
}

pub enum Returnable {
    Result(MeasResult),
    None,
}

/// These are just mock operations to return the signal
pub enum GateType {
    X,
    Y,
    Z,
    H,
    S,
    T,
    CxControl,
    CxTarget,
    CzControl,
    CzTarget,
}
