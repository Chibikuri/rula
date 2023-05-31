use crate::inst;
use crate::types::*;
use crate::InstructionInfo;
use crate::RSIR;

// These inst!uctions are expanded to the structures
// The inst!uction named with "_DUP" will be renamed into a name without "_DUP" for QuISP
// READ and WRITE are just annotations to know which arguments are written or read

// arithmetic
inst!(ADD, WRITE reg0: RegId, READ reg1: RegId, READ val: i64);
inst!(ADD_DUP, WRITE reg0: RegId, READ reg1: RegId, READ reg2: RegId); // In QuISP, this is the same as `Add`
inst!(SUB, WRITE reg0: RegId, READ reg1: RegId, READ val: i64);
inst!(SUB_DUP, WRITE reg0: RegId, READ reg1: RegId, READ reg2: RegId); // In QuISP, this is the same as `Sub`
inst!(INC, WR reg0: RegId);
inst!(SET, WRITE reg0: RegId, READ val: i64);
// arithmetic bitwise
inst!(BITWISE_AND, WRITE reg0: RegId, READ reg1: RegId, READ val: i64);
inst!(BITWISE_AND_DUP, WRITE reg0: RegId, READ reg1: RegId, READ reg2: RegId); // In QuISP, this is the same as `BITWISE_AND`
inst!(BITWISE_OR, WRITE reg0: RegId, READ reg1: RegId, READ val: i64);
inst!(BITWISE_OR_DUP, WRITE reg0: RegId, READ reg1: RegId, READ reg2: RegId);
inst!(BITWISE_XOR, WRITE reg0: RegId, READ reg1: RegId, READ reg2: RegId);
inst!(BITWISE_XOR_DUP, WRITE reg0: RegId, READ reg1: RegId, READ val: i64);
// in place operation
inst!(BITWISE_AND_INP, WRITE reg0: RegId, READ reg1: RegId ); // in-place operation: first_reg = first_reg | second_reg (bitwise and);
inst!(BITWISE_AND_INP_DUP, WRITE reg0: RegId , READ val: i64 ); //  in-place operation: first_reg = first_reg | int (bitwise and);
inst!(BITWISE_OR_INP, WRITE reg0: RegId , READ reg1: RegId ); //  in-place operation: first_reg = first_reg | second_reg (bitwise or);
inst!(BITWISE_OR_INP_DUP, WRITE reg0: RegId, READ val: i64 ); //  in-place operation: first_reg = first_reg | int (bitwise or);
inst!(BITWISE_XOR_INP, WRITE reg0: RegId, READ reg1: RegId ); //  in-place operation: first_reg = first_reg | second_reg (bitwise xor);
inst!(BITWISE_XOR_INP_DUP, WRITE reg0: RegId, READ reg1: i64 ); //  in-place operation: first_reg = first_reg | int (bitwise xor);

// control flow
inst!(BEQ, LABEL label: Label, READ reg0: RegId, READ reg1: RegId); // branch if the reg values are same
inst!(BEQ_DUP, LABEL label: Label, READ reg0: RegId, READ val: i64); // branch if the reg value is equal to the int value
inst!(BEZ, LABEL label: Label, READ reg0: RegId); // branch if the reg value is zero
inst!(BNZ, LABEL label: Label, READ reg0: RegId); // branch if the reg value is not zero
inst!(BLT, LABEL label: Label, READ reg0: RegId, READ val: i64); // branch if the reg value is less than the int value
inst!(BRANCH_IF_LOCKED, LABEL label: Label, READ reg: RegId);
inst!(BRANCH_IF_QUBIT_FOUND, LABEL label: Label);
inst!(BRANCH_IF_MESSAGE_FOUND, LABEL label: Label);
inst!(JMP, LABEL label: Label);
inst!(ERROR, message: String); // stop execution and show error
inst!(RET, code: ReturnCode); // stop execution with the ReturnCode

// memory operations
inst!(LOAD, WRITE reg0: RegId, READ mem_key: MemoryKey);
inst!(STORE, WRITE mem_key: MemoryKey, READ reg0: RegId);
inst!(STORE_DUP, WRITE mem_key: MemoryKey, READ val: i64);

inst!(LOAD_LEFT_OP, WRITE reg0: RegId, READ mem_key: MemoryKey); // READ memkey?
inst!(LOAD_RIGHT_OP, WRITE reg0: RegId, READ mem_key: MemoryKey);

// qubit retrieval operations
inst!(GET_QUBIT, WRITE qubit0: QubitId, READ qnode_addr0: QNodeAddr, READ val: i64); // may throw "no qubit error"
inst!(GET_QUBIT_R_DUP, WRITE qubit0: QubitId, READ partner_addr: QNodeAddr, READ reg0: RegId); // may throw "no qubit error"
inst!(GET_QUBIT_RQ_DUP, WRITE reg0: RegId, READ partner_addr: QNodeAddr, READ qubit_index: RegId);
inst!(GET_QUBIT_BY_SEQ_NO, WRITE reg0: RegId, READ partner_addr: QNodeAddr, READ sequence_number: RegId);
inst!(GET_QUBIT_BY_SEQ_NO_DUP, WRITE qubit: QubitId, READ partner_addr: QNodeAddr, READ sequence_number: RegId);

// qubit quantum gate operations
inst!(MEASURE_RANDOM, WRITE reg0: MemoryKey, READ target_qubit: QubitId);
inst!(MEASURE, WRITE reg0: MemoryKey, READ target_qubit: QubitId, READ meas_basis: Basis);
inst!(MEASURE_R_DUP, WRITE reg0: RegId, READ target_qubit: QubitId, READ meas_basis: Basis);
inst!(MEASURE_RQ_DUP, WRITE reg0: RegId, READ bit: i64, READ target_qubit: QubitId, READ meas_basis: Basis); // this will store the result at index (bitset); of the RegId specified by the int
inst!(GATE_X, READ target_qubit: QubitId);
inst!(GATE_Z, READ target_qubit: QubitId);
inst!(GATE_Y, READ target_qubit: QubitId);
inst!(GATE_CNOT, READ control_qubit: QubitId, READ target_qubit: QubitId);
// circuit operations
inst!(PURIFY_X, WRITE reg0: RegId, READ val: i64, READ keep_qubit_id: QubitId, READ trash_qubit_id: QubitId);
inst!(PURIFY_Z, WRITE reg0: RegId, READ val: i64, READ keep_qubit_id: QubitId, READ trash_qubit_id: QubitId);
inst!(PURIFY_Y, WRITE reg0: RegId, READ val: i64, READ keep_qubit_id: QubitId, READ trash_qubit_id: QubitId);

// resource management operations
// inst!uctions we would want later: crucial for entanglement pumping, banding, and multipartite states
// inst!(SET_NAME, QubitId, {new_name: string | RegId }); // when using partner as name is not enough
inst!(FREE_QUBIT, READ qubit: QubitId);
inst!(PROMOTE, READ qubit: QubitId);
inst!(PROMOTE_DUP, READ qubit: QubitId, READ reg0: RegId); // promote with new partner/new name
inst!(LOCK_QUBIT, READ qubit: QubitId, READ reg0_action_index: RegId);

// message operations
inst!(GET_MESSAGE_SEQ, WRITE sequence_number: RegId, READ message_index: RegId);
inst!(COUNT_MESSAGE, WRITE count: RegId, READ sequence_number: RegId);
inst!(GET_MESSAGE, WRITE reg_content: RegId, READ sequence_number: RegId, READ message_index: i64); // for purification [result]
inst!(GET_MESSAGE_DUP, WRITE content0: RegId, WRITE content1: RegId, READ sequence_number: RegId, READ message_index: i64); // for swapping [correction_op, new_partner]
inst!(DELETE_MESSAGE, READ sequence_number: RegId); // delete all messages with this sequence number

// send classical messages
inst!(SEND_LINK_TOMOGRAPHY_RESULT, READ partner_addr: QNodeAddr, READ reg0: RegId, READ mem_key: MemoryKey, READ val: i64, READ time: Time); // partner addr, current count reg_id, outcome key, max_count, start_time
inst!(SEND_PURIFICATION_RESULT, READ partner_addr: QNodeAddr, READ reg0: RegId /* measurement_result encoded in int */, READ reg1: RegId /* sequence_number */, READ pur_type: PurType);
inst!(SEND_SWAPPING_RESULT, READ partner_addr: QNodeAddr /* receipient */, READ reg0: RegId /* pauli_op */, READ new_partner_addr: QNodeAddr /* new partner*/, READ sequence_number: RegId /* sequence_number */);
inst!(NOP, READ nop: Option<Nop>);
