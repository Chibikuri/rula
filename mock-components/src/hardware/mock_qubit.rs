use super::error::HardwareError;
use super::result::{MeasBasis, MeasResult, Outcome};
use super::IResult;
use serde::{Deserialize, Serialize};
use std::net::IpAddr;
use tokio::time::{sleep, Duration};

pub enum QubitInstruction {
    SetFree,
    SetBusy,
    CheckStatus,
    Gate(GateType),
    Measure(MeasBasis),
    SetEntangled(IpAddr),
    Test,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct MockQubit {
    pub address: u64,
    pub(crate) busy: bool,
    pub(crate) entangled_with: Option<IpAddr>,
}

impl MockQubit {
    pub fn new(address: u64) -> Self {
        MockQubit {
            address: address,
            busy: false,
            entangled_with: None,
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

    fn free(&mut self) -> IResult<()> {
        if !self.busy {
            return Err(HardwareError::QubitNotBusy);
        }
        self.busy = false;
        Ok(())
    }

    fn busy(&mut self) -> IResult<()> {
        if self.busy {
            return Err(HardwareError::QubitAlreadyBusy);
        }
        self.busy = true;
        Ok(())
    }

    fn check_status(&mut self) -> bool {
        self.busy
    }

    async fn measure(&mut self, meas_basis: &MeasBasis) -> IResult<Returnable> {
        sleep(Duration::from_nanos(200)).await;
        // Here this just always resutns one for mocking reason
        match meas_basis {
            MeasBasis::X => Ok(Returnable::MeasResult(MeasResult::new(
                MeasBasis::X,
                Outcome::One,
            ))),
            MeasBasis::Y => Ok(Returnable::MeasResult(MeasResult::new(
                MeasBasis::Y,
                Outcome::One,
            ))),
            MeasBasis::Z => Ok(Returnable::MeasResult(MeasResult::new(
                MeasBasis::Z,
                Outcome::One,
            ))),
            _ => return Err(HardwareError::InvalidBasis),
        }
    }

    fn set_entangled_with(&mut self, entangled_with: Option<IpAddr>) -> IResult<()> {
        self.entangled_with = entangled_with;
        Ok(())
    }

    pub async fn call_instruction(&mut self, instruction: QubitInstruction) -> IResult<Returnable> {
        match instruction {
            QubitInstruction::SetFree => {
                self.free().unwrap();
                Ok(Returnable::None)
            }
            QubitInstruction::SetBusy => {
                self.busy().unwrap();
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
            QubitInstruction::SetEntangled(partner_addr) => {
                MockQubit::set_entangled_with(self, Some(partner_addr)).unwrap();
                Ok(Returnable::None)
            }
            _ => todo!("unknown instruction"),
        }
    }
}

pub enum Returnable {
    MeasResult(MeasResult),
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
