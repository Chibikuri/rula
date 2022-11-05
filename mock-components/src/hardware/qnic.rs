use super::error::HardwareError;
use super::qubit::{GateType, MockQubit, QubitInstruction, Returnable};
use super::result::{MeasBasis, MeasResult, Outcome};
use super::IResult;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::net::IpAddr;

pub struct MockQnic {
    /// `qubits` can only be accessed by proper function calls
    qubits: HashMap<u64, MockQubit>,
    register: HashMap<u64, MeasResult>,
    /// Index for qubit
    index: u64,
    /// Basic qnic information.
    qnic_type: Option<QnicType>,
    qnic_id: Option<u32>,
    qnic_address: Option<IpAddr>,
}

#[derive(Debug)]
pub enum QnicInstruction {
    GetQnicInfo,
    /// Free specified qubit
    FreeQubit(QubitAddress),
    /// Get ownership of qubit
    GetQubit(QubitAddress),
    /// Get Qubit Status,
    GetQubitStatus(QubitAddress),
    /// Gate operation
    ApplyGate(ApplyGate),
    /// Measurement operation
    MeasureQubit(MeasureQubit),
    /// Send message (This might be in different place)
    Send(Send),
    /// Load result
    Load(Load),
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
            qnic_type: None,
            qnic_id: None,
            qnic_address: None,
        }
    }
    /// free target qubit so that next rule can use that qubit
    async fn free(&mut self, free_qubit: &QubitAddress) -> IResult<()> {
        if MockQnic::exist(self, free_qubit.address) {
            let qubit = self.qubits.get_mut(&free_qubit.address).unwrap();
            qubit
                .call_instruction(QubitInstruction::SetFree)
                .await
                .unwrap();
            Ok(())
        } else {
            Err(HardwareError::NoQubitFound)
        }
    }

    /// get qubit and reserve it for later use (Here is just a mock, nothing is happening)
    async fn get(&mut self, get_qubit: &QubitAddress) -> IResult<()> {
        if MockQnic::exist(self, get_qubit.address) {
            let qubit = self.qubits.get_mut(&get_qubit.address).unwrap();
            qubit
                .call_instruction(QubitInstruction::SetBusy)
                .await
                .unwrap();
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
                                .await
                                .unwrap();

                            let target_qubit = self.qubits.get_mut(&apply_gate.target).unwrap();
                            target_qubit
                                .call_instruction(QubitInstruction::Gate(GateType::CxTarget))
                                .await
                                .unwrap();
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
                                .await
                                .unwrap();
                            // apply mock cx target
                            let target_qubit = self.qubits.get_mut(&apply_gate.target).unwrap();
                            target_qubit
                                .call_instruction(QubitInstruction::Gate(GateType::CzTarget))
                                .await
                                .unwrap();
                        }
                        None => return Err(HardwareError::NoControlQubitFound),
                    }
                }
                QuantumGate::X => {
                    let target = self.qubits.get_mut(&apply_gate.target).unwrap();
                    target
                        .call_instruction(QubitInstruction::Gate(GateType::X))
                        .await
                        .unwrap();
                }
                QuantumGate::Y => {
                    let target = self.qubits.get_mut(&apply_gate.target).unwrap();
                    target
                        .call_instruction(QubitInstruction::Gate(GateType::Y))
                        .await
                        .unwrap();
                }
                QuantumGate::Z => {
                    let target = self.qubits.get_mut(&apply_gate.target).unwrap();
                    target
                        .call_instruction(QubitInstruction::Gate(GateType::Z))
                        .await
                        .unwrap();
                }
                QuantumGate::H => {
                    let target = self.qubits.get_mut(&apply_gate.target).unwrap();
                    target
                        .call_instruction(QubitInstruction::Gate(GateType::H))
                        .await
                        .unwrap();
                }
                QuantumGate::S => {
                    let target = self.qubits.get_mut(&apply_gate.target).unwrap();
                    target
                        .call_instruction(QubitInstruction::Gate(GateType::S))
                        .await
                        .unwrap();
                }
                QuantumGate::T => {
                    let target = self.qubits.get_mut(&apply_gate.target).unwrap();
                    target
                        .call_instruction(QubitInstruction::Gate(GateType::T))
                        .await
                        .unwrap();
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
                match qubit
                    .call_instruction(QubitInstruction::Measure(MeasBasis::X))
                    .await
                    .unwrap()
                {
                    Returnable::MeasResult(result) => {
                        self.register.insert(measure_qubit.register_address, result);
                    }
                    _ => return Err(HardwareError::NoMeasurementFound),
                };
            }
            MeasBasis::Y => {
                match qubit
                    .call_instruction(QubitInstruction::Measure(MeasBasis::Y))
                    .await
                    .unwrap()
                {
                    Returnable::MeasResult(result) => {
                        self.register.insert(measure_qubit.register_address, result);
                    }
                    _ => return Err(HardwareError::NoMeasurementFound),
                };
            }
            MeasBasis::Z => {
                match qubit
                    .call_instruction(QubitInstruction::Measure(MeasBasis::Z))
                    .await
                    .unwrap()
                {
                    Returnable::MeasResult(result) => {
                        self.register.insert(measure_qubit.register_address, result);
                    }
                    _ => return Err(HardwareError::NoMeasurementFound),
                };
            }
            _ => return Err(HardwareError::InvalidBasis),
        }
        Ok(())
    }

    async fn send(&mut self, send: &Send) -> IResult<()> {
        // How to do this?
        Ok(())
    }

    // Load measurement result from register for the result comparison
    async fn load(&mut self, load: &Load) -> IResult<&MeasResult> {
        let result = self.register.get(&load.register_address).unwrap();
        Ok(result)
    }

    async fn check_entangled_with(
        &mut self,
        entangled_with: &CheckEntangledWith,
    ) -> IResult<IpAddr> {
        let qubit = self.qubits.get(&entangled_with.qubit_address).unwrap();
        Ok(qubit.entangled_with.unwrap())
    }

    fn exist(&mut self, qubit_address: u64) -> bool {
        match self.qubits.get(&qubit_address) {
            Some(_qubit) => true,
            _ => false,
        }
    }

    async fn info(&mut self) -> IResult<QnicInfo> {
        Ok(QnicInfo::new(
            self.qnic_type.clone(),
            self.qnic_id,
            self.qnic_address,
            self.qubits.len() as u32,
        ))
    }

    // using trait object would be easier?
    pub async fn call_instruction(
        &mut self,
        instruction: QnicInstruction,
    ) -> IResult<QnicReturnable> {
        match instruction {
            QnicInstruction::GetQnicInfo => {
                let qnic_info = MockQnic::info(self).await.unwrap();
                Ok(QnicReturnable::QnicInfo(qnic_info))
            }
            QnicInstruction::FreeQubit(free_qubit) => {
                MockQnic::free(self, &free_qubit).await.unwrap();
                Ok(QnicReturnable::None)
            }
            QnicInstruction::GetQubit(get_qubit) => {
                MockQnic::get(self, &get_qubit).await.unwrap();
                Ok(QnicReturnable::None)
            }
            QnicInstruction::ApplyGate(apply_gate) => {
                MockQnic::apply(self, &apply_gate).await.unwrap();
                Ok(QnicReturnable::None)
            }
            QnicInstruction::MeasureQubit(meas_qubit) => {
                MockQnic::measure(self, &meas_qubit).await.unwrap();
                Ok(QnicReturnable::None)
            }
            QnicInstruction::Send(send) => {
                MockQnic::send(self, &send).await.unwrap();
                Ok(QnicReturnable::None)
            }
            QnicInstruction::Load(load) => {
                let result = MockQnic::load(self, &load).await.unwrap();
                Ok(QnicReturnable::MeasResult(result.clone()))
            }
            QnicInstruction::CheckEntangledWith(entangled_with) => {
                MockQnic::check_entangled_with(self, &entangled_with)
                    .await
                    .unwrap();
                Ok(QnicReturnable::None)
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

// Do we need this?
#[derive(Debug, PartialEq)]
pub enum QnicReturnable {
    MeasResult(MeasResult),
    QnicInfo(QnicInfo),
    None,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum QnicType {
    QnicE,
    QnicP,
    QnicRp,
    QnicN, // place holder
}

#[derive(Debug, PartialEq)]
pub struct QnicInfo {
    pub qnic_type: Option<QnicType>,
    pub qnic_id: Option<u32>,
    pub qnic_address: Option<IpAddr>,
    pub num_qubit: u32,
}

impl QnicInfo {
    pub fn new(
        qnic_type: Option<QnicType>,
        qnic_id: Option<u32>,
        qnic_address: Option<IpAddr>,
        num_qubit: u32,
    ) -> Self {
        QnicInfo {
            qnic_type: qnic_type,
            qnic_id: qnic_id,
            qnic_address: qnic_address,
            num_qubit: num_qubit,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct QubitAddress {
    pub address: u64,
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
pub struct Send {
    pub register_address: u64,
    pub destination: IpAddr,
}

#[derive(Debug)]
pub struct Load {
    pub register_address: u64,
}

#[derive(Debug)]
pub struct CheckEntangledWith {
    pub qubit_address: u64,
}

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
        let set_busy_instruction = QnicInstruction::GetQubit(QubitAddress { address: 0 });
        qnic.call_instruction(set_busy_instruction).await.unwrap();
        let target = qnic.qubits.get(&0).unwrap();
        assert_eq!(target.busy, true);
    }

    #[tokio::test]
    async fn test_set_free_qubit() {
        let mut qnic = MockQnic::new();
        let mock_qubit = MockQubit::new(0);
        qnic.append_qubits(mock_qubit);

        let target = qnic.qubits.get(&0).unwrap();
        assert_eq!(target.busy, false);

        let set_busy_instruction = QnicInstruction::GetQubit(QubitAddress { address: 0 });
        qnic.call_instruction(set_busy_instruction).await.unwrap();
        let updated_target = qnic.qubits.get(&0).unwrap();
        assert_eq!(updated_target.busy, true);

        let set_free_instruction = QnicInstruction::FreeQubit(QubitAddress { address: 0 });
        qnic.call_instruction(set_free_instruction).await.unwrap();
        let updated_target = qnic.qubits.get(&0).unwrap();
        assert_eq!(updated_target.busy, false);
    }

    #[tokio::test]
    async fn test_gate_operation() {
        let mut qnic = MockQnic::new();
        let mock_qubit = MockQubit::new(0);
        qnic.append_qubits(mock_qubit);

        let gate_operation = QnicInstruction::ApplyGate(ApplyGate {
            control: None,
            target: 0,
            gate: QuantumGate::X,
        });
        let res = qnic.call_instruction(gate_operation).await.unwrap();
        assert_eq!(res, QnicReturnable::None);
    }

    #[tokio::test]
    async fn test_measure_qubit() {
        let mut qnic = MockQnic::new();
        let mock_qubit = MockQubit::new(0);
        qnic.append_qubits(mock_qubit);

        let measure_instruction = QnicInstruction::MeasureQubit(MeasureQubit {
            qubit_address: 0,
            register_address: 0,
            basis: MeasBasis::X,
        });
        qnic.call_instruction(measure_instruction).await.unwrap();
        let load_instruction = QnicInstruction::Load(Load {
            register_address: 0,
        });
        let result = qnic.call_instruction(load_instruction).await.unwrap();
        assert_eq!(
            result,
            QnicReturnable::MeasResult(MeasResult {
                basis: MeasBasis::X,
                result: Outcome::One
            })
        );

        let mock_qubit = MockQubit::new(1);
        qnic.append_qubits(mock_qubit);
        let measure_instruction = QnicInstruction::MeasureQubit(MeasureQubit {
            qubit_address: 1,
            register_address: 1,
            basis: MeasBasis::Y,
        });
        qnic.call_instruction(measure_instruction).await.unwrap();
        let load_instruction = QnicInstruction::Load(Load {
            register_address: 1,
        });
        let result = qnic.call_instruction(load_instruction).await.unwrap();
        assert_eq!(
            result,
            QnicReturnable::MeasResult(MeasResult {
                basis: MeasBasis::Y,
                result: Outcome::One
            })
        );

        let mock_qubit = MockQubit::new(2);
        qnic.append_qubits(mock_qubit);
        let measure_instruction = QnicInstruction::MeasureQubit(MeasureQubit {
            qubit_address: 2,
            register_address: 2,
            basis: MeasBasis::Z,
        });
        qnic.call_instruction(measure_instruction).await.unwrap();
        let load_instruction = QnicInstruction::Load(Load {
            register_address: 2,
        });
        let result = qnic.call_instruction(load_instruction).await.unwrap();
        assert_eq!(
            result,
            QnicReturnable::MeasResult(MeasResult {
                basis: MeasBasis::Z,
                result: Outcome::One
            })
        );
    }

    // #[test]
}
