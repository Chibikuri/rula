use super::error::HardwareError;
use super::qubit::{GateType, MockQubit, QubitInstruction};
use super::result::{MeasBasis, MeasResult};
use super::IResult;
use std::collections::HashMap;
use std::fmt;

pub struct MockQnic {
    /// `qubits` can only be accessed by proper function calls
    qubits: HashMap<u64, MockQubit>,
    register: HashMap<u64, MeasResult>,
    index: u64,
}

#[derive(Debug)]
pub enum QnicInstruction {
    /// Free specified qubit
    FreeQubit(FreeQubit),
    /// Get ownership of qubit
    GetQubit(GetQubit),
    /// Emit photons from Quantum Memory (This is L2)
    // EmitPhoton(EmitPhoton),
    /// Gate operation
    ApplyGate(ApplyGate),
    /// Measurement operation
    MeasureQubit(MeasureQubit),
    /// Send message (This might be in different place)
    Send(Send),
    /// Store result
    Store(Store),
    /// Check entangled partner
    CheckEntangledWith(CheckEntangledWith),
    /// No operation just in case,
    Nop,
}

impl MockQnic {
    pub fn new() -> Self {
        MockQnic {
            qubits: HashMap::new(),
            register: HashMap::new(),
            index: 0,
        }
    }
    /// free target qubit
    async fn free(&mut self, free_qubit: &FreeQubit) -> IResult<()> {
        if MockQnic::exist(self, free_qubit.qubit_address) {
            let qubit = self.qubits.get_mut(&free_qubit.qubit_address).unwrap();
            qubit.call_instruction(QubitInstruction::SetFree).await;
            Ok(())
        } else {
            Err(HardwareError::NoQubitFound)
        }
    }

    async fn reserve(&mut self, get_qubit: &GetQubit) -> IResult<()> {
        if MockQnic::exist(self, get_qubit.qubit_address) {
            let qubit = self.qubits.get_mut(&get_qubit.qubit_address).unwrap();
            qubit.call_instruction(QubitInstruction::SetBusy).await;
            Ok(())
        } else {
            Err(HardwareError::NoQubitFound)
        }
    }

    async fn apply(&mut self, apply_gate: &ApplyGate) -> IResult<()> {
        if MockQnic::exist(self, apply_gate.target) {
            match apply_gate.gate {
                QuantumGate::Cx => {
                    match apply_gate.control {
                        Some(control_qubit) => {
                            // apply mock cx control
                            let cqubit = self.qubits.get_mut(&control_qubit).unwrap();
                            cqubit
                                .call_instruction(QubitInstruction::Gate(GateType::CxControl))
                                .await;

                            let target_qubit = self.qubits.get_mut(&apply_gate.target).unwrap();
                            target_qubit
                                .call_instruction(QubitInstruction::Gate(GateType::CxTarget))
                                .await;
                        }
                        None => return Err(HardwareError::NoControlQubitFound),
                    }
                }
                QuantumGate::Cz => {
                    match apply_gate.control {
                        Some(control_qubit) => {
                            // apply mock cx control
                            let cqubit = self.qubits.get_mut(&control_qubit).unwrap();
                            cqubit
                                .call_instruction(QubitInstruction::Gate(GateType::CzControl))
                                .await;
                            // apply mock cx target
                            let target_qubit = self.qubits.get_mut(&apply_gate.target).unwrap();
                            target_qubit
                                .call_instruction(QubitInstruction::Gate(GateType::CzTarget))
                                .await;
                        }
                        None => return Err(HardwareError::NoControlQubitFound),
                    }
                }
                QuantumGate::X => {
                    let target = self.qubits.get_mut(&apply_gate.target).unwrap();
                    target
                        .call_instruction(QubitInstruction::Gate(GateType::X))
                        .await;
                }
                QuantumGate::Y => {
                    let target = self.qubits.get_mut(&apply_gate.target).unwrap();
                    target
                        .call_instruction(QubitInstruction::Gate(GateType::Y))
                        .await;
                }
                QuantumGate::Z => {
                    let target = self.qubits.get_mut(&apply_gate.target).unwrap();
                    target
                        .call_instruction(QubitInstruction::Gate(GateType::Z))
                        .await;
                }
                QuantumGate::H => {
                    let target = self.qubits.get_mut(&apply_gate.target).unwrap();
                    target
                        .call_instruction(QubitInstruction::Gate(GateType::H))
                        .await;
                }
                QuantumGate::S => {
                    let target = self.qubits.get_mut(&apply_gate.target).unwrap();
                    target
                        .call_instruction(QubitInstruction::Gate(GateType::S))
                        .await;
                }
                QuantumGate::T => {
                    let target = self.qubits.get_mut(&apply_gate.target).unwrap();
                    target
                        .call_instruction(QubitInstruction::Gate(GateType::T))
                        .await;
                }
            }
        } else {
            return Err(HardwareError::NoQubitFound);
        }
        Ok(())
    }

    async fn measure(&mut self, measure_qubit: &MeasureQubit) -> IResult<()> {
        let qubit = self.qubits.get_mut(&measure_qubit.qubit_address).unwrap();
        match measure_qubit.basis {
            MeasBasis::X => {
                qubit
                    .call_instruction(QubitInstruction::Measure(MeasBasis::X))
                    .await;
            }
            MeasBasis::Y => {
                qubit
                    .call_instruction(QubitInstruction::Measure(MeasBasis::Y))
                    .await;
            }
            MeasBasis::Z => {
                qubit
                    .call_instruction(QubitInstruction::Measure(MeasBasis::Z))
                    .await;
            }
        }
        Ok(())
    }

    async fn send(&mut self, send: &Send) -> IResult<()> {
        Ok(())
    }

    async fn store(&mut self, store_result: &Store) -> IResult<()> {
        Ok(())
    }

    async fn check_entangled_with(&mut self, entangled_with: &CheckEntangledWith) -> IResult<()> {
        Ok(())
    }

    fn exist(&mut self, qubit_address: u64) -> bool {
        match self.qubits.get(&qubit_address) {
            Some(_qubit) => true,
            _ => false,
        }
    }

    // using trait object would be easier?
    pub async fn call_instruction(&mut self, instruction: QnicInstruction) -> IResult<()> {
        match instruction {
            QnicInstruction::FreeQubit(free_qubit) => {
                MockQnic::free(self, &free_qubit).await.unwrap();
                Ok(())
            }
            QnicInstruction::GetQubit(get_qubit) => {
                MockQnic::reserve(self, &get_qubit).await.unwrap();
                Ok(())
            }
            QnicInstruction::ApplyGate(apply_gate) => {
                MockQnic::apply(self, &apply_gate).await.unwrap();
                Ok(())
            }
            QnicInstruction::MeasureQubit(meas_qubit) => {
                MockQnic::measure(self, &meas_qubit).await.unwrap();
                Ok(())
            }
            QnicInstruction::Send(send) => {
                MockQnic::send(self, &send).await.unwrap();
                Ok(())
            }
            QnicInstruction::Store(store) => {
                MockQnic::store(self, &store).await.unwrap();
                Ok(())
            }
            QnicInstruction::CheckEntangledWith(entangled_with) => {
                MockQnic::check_entangled_with(self, &entangled_with)
                    .await
                    .unwrap();
                Ok(())
            }
            _ => Err(HardwareError::NoInstructionFound),
        }
    }

    pub fn append_qubits(&mut self, qubit: MockQubit) {
        self.qubits.insert(self.index, qubit);
        self.index += 1;
        // self.qubits.push(qubit);
    }
}

pub enum Returnable {
    Qubit(Box<MockQubit>),
    None,
}

#[derive(Debug)]
pub struct FreeQubit {
    pub qubit_address: u64,
}

#[derive(Debug)]
pub struct GetQubit {
    pub qubit_address: u64,
}

#[derive(Debug)]
pub struct ApplyGate {
    pub control: Option<u64>,
    pub target: u64,
    pub gate: QuantumGate,
}

#[derive(Debug)]
pub enum QuantumGate {
    X,
    Y,
    Z,
    H,
    S,
    T,
    Cx,
    Cz,
}

#[derive(Debug)]
pub struct MeasureQubit {
    pub qubit_address: u64,
    pub register_address: u64,
    pub basis: MeasBasis,
}

#[derive(Debug)]
pub struct Send {}

#[derive(Debug)]
pub struct Store {}

#[derive(Debug)]
pub struct CheckEntangledWith {}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[tokio::test]
    async fn test_set_busy_qubit() {
        // Target instruction
        let mut qnic = MockQnic::new();
        let mock_qubit = MockQubit::new(0);
        qnic.append_qubits(mock_qubit);
        // Get qubit to make it busy
        let set_busy_instruction = QnicInstruction::GetQubit(GetQubit { qubit_address: 0 });
        qnic.call_instruction(set_busy_instruction).await.unwrap();
        // assert_eq!(qnic.qubits.get(&0).unwrap(), false);
    }
}
