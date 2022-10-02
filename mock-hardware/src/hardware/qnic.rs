use super::qubit::MockQubit;

pub struct MockQnic {
    /// `qubits` can only be accessed by proper function calls
    qubits: Vec<MockQubit>,
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
    /// No operation just in case,
    Nop,
}

impl MockQnic {
    pub fn new() -> Self {
        MockQnic { qubits: vec![] }
    }
    /// free target qubit
    fn free(&self, free_qubit: &FreeQubit) {}

    fn get(&self, get_qubit: &GetQubit) {}

    fn apply(&self, apply_gate: &ApplyGate) {}

    fn measure(&self, measure_qubit: &MeasureQubit) {}

    fn send(&self, send: &Send) {}

    fn store(&self, store_result: &Store) {}

    pub fn call_instruction(&self, instruction: QnicInstruction) {
        match instruction {
            QnicInstruction::FreeQubit(free_qubit) => MockQnic::free(self, &free_qubit),
            QnicInstruction::GetQubit(get_qubit) => MockQnic::get(self, &get_qubit),
            QnicInstruction::ApplyGate(apply_gate) => MockQnic::apply(self, &apply_gate),
            QnicInstruction::MeasureQubit(meas_qubit) => MockQnic::measure(self, &meas_qubit),
            QnicInstruction::Send(send) => MockQnic::send(self, &send),
            QnicInstruction::Store(store) => MockQnic::store(&self, &store),
            _ => todo!("new instruction?"),
        }
    }

    fn append_qubits(&mut self, qubit: MockQubit) {
        self.qubits.push(qubit);
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

#[cfg(test)]
pub mod tests {
    use super::*;
}
