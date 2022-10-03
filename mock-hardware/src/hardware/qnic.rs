use super::error::HardwareError;
use super::qubit::MockQubit;
use super::IResult;
use std::collections::HashMap;

pub struct MockQnic {
    /// `qubits` can only be accessed by proper function calls
    qubits: HashMap<u64, MockQubit>,
    index: u64,
}

pub enum QnicInstruction {
    /// Free specified qubit
    FreeQubit(FreeQubit),
    /// Get qubit
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
            index: 0,
        }
    }
    /// free target qubit
    fn free(&self, free_qubit: &FreeQubit) -> IResult<()>{
        if MockQnic::exist(self, free_qubit.qubit_address){
            Ok(())
        }else{
            Err(HardwareError::Test)
        }
    }

    fn get(&self, get_qubit: &GetQubit) -> IResult<()> {
        Ok(())
    }

    fn apply(&self, apply_gate: &ApplyGate) -> IResult<()>{
        Ok(())
    }

    fn measure(&self, measure_qubit: &MeasureQubit)-> IResult<()> {
        Ok(())
    }

    fn send(&self, send: &Send) -> IResult<()>{
        Ok(())
    }

    fn store(&self, store_result: &Store) -> IResult<()>{
        Ok(())
    }

    fn check_entangled_with(&self, entangled_with: &CheckEntangledWith) -> IResult<()>{
        Ok(())
    }

    fn exist(&self, qubit_address: u64) -> bool {
        match self.qubits.get(&qubit_address) {
            Some(_qubit) => true,
            _ => false,
        }
    }

    pub fn call_instruction(&self, instruction: QnicInstruction) -> IResult<()>{
        match instruction {
            QnicInstruction::FreeQubit(free_qubit) => MockQnic::free(self, &free_qubit),
            QnicInstruction::GetQubit(get_qubit) => MockQnic::get(self, &get_qubit),
            QnicInstruction::ApplyGate(apply_gate) => MockQnic::apply(self, &apply_gate),
            QnicInstruction::MeasureQubit(meas_qubit) => MockQnic::measure(self, &meas_qubit),
            QnicInstruction::Send(send) => MockQnic::send(self, &send),
            QnicInstruction::Store(store) => MockQnic::store(&self, &store),
            QnicInstruction::CheckEntangledWith(entangled_with) => {
                MockQnic::check_entangled_with(&self, &entangled_with)
            }
            _ => todo!("new instruction?"),
        }
    }

    pub fn append_qubits(&mut self, qubit: MockQubit) {
        self.qubits.insert(self.index, qubit);
        self.index += 1;
        // self.qubits.push(qubit);
    }
}

pub struct FreeQubit {
    pub qubit_address: u64,
}

pub struct EmitPhoton {}

pub struct ApplyGate {}

pub struct GetQubit {}

pub struct MeasureQubit {}

pub struct Send {}
pub struct Store {}

pub struct CheckEntangledWith {}
#[cfg(test)]
pub mod tests {
    use super::*;
}
