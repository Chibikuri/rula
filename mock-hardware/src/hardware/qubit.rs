use tokio::time::{sleep, Duration};

pub enum QubitInstruction {
    SetFree,
    SetBusy,
    CheckStatus,
    Gate(GateType),
    Measure(Measure),
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

    pub async fn call_instruction(&mut self, instruction: QubitInstruction) {
        match instruction {
            QubitInstruction::SetFree => {
                self.free();
            }
            QubitInstruction::SetBusy => {
                self.busy();
            }
            QubitInstruction::CheckStatus => {
                self.check_status();
            }
            QubitInstruction::Gate(gate_type) => {
                MockQubit::gate(self, &gate_type).await;
            }
            _ => todo!("unknown instruction"),
        }
    }
}

pub struct Measure {
    pub basis: MeasurementBasis,
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

pub enum MeasurementBasis {
    X,
    Y,
    Z,
}
