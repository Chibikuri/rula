#[allow(dead_code)]
#[allow(non_camel_case_types)]
pub mod instructions;

pub mod types;

// A structure that stores a set of instructions defined in instructions
pub struct RuleSetIR {
    instructions: Vec<Box<dyn RSIR>>,
}

impl RuleSetIR {
    pub fn new() -> Self {
        RuleSetIR {
            instructions: vec![],
        }
    }

    pub fn add_instruction(&mut self, instruction: Box<dyn RSIR>) {
        self.instructions.push(instruction)
    }

    pub fn merge(&mut self, ruleset_ir: &mut RuleSetIR) {
        // read rulesets and push back one by one
        self.instructions.append(&mut ruleset_ir.instructions);
    }

    pub fn export_ir(&self) -> String {
        let mut ruleset_ir = String::from("");
        for inst in self.instructions.iter() {
            ruleset_ir = ruleset_ir + &inst.export() + "\n";
        }
        ruleset_ir
    }

    pub fn import_ir(&self, _ruleset_ir: String) {}
}

// All the ruleset instructions needs to implement this trait RuleSet IR (RSIR).
// This trait implements `gen_ir` that returns a text format instruction for RuleSet
pub trait RSIR {
    fn export(&self) -> String;
    fn import(&self, ir: String);
}

#[macro_export]
macro_rules! inst {
    // with annotation
    ($inst_name: ident, $($mode: ident $args: ident: $typedef: ty),* ) => {
        #[derive(Debug)]
        pub struct $inst_name {
            $(
                $args: $typedef,
            )*
        }

        impl $inst_name {
            #[allow(dead_code)]
            pub fn new($($args: $typedef),*) -> Self {
                $inst_name {
                    $(
                        $args: $args,
                    )*
                }
            }
        }

        impl RSIR for $inst_name {
            fn export(&self) -> String{
                String::from("")
            }

            fn import(&self, _ir: String){
                todo!("IR importing is under construction")
            }

        }
    };
    // without annotation (need refactor)
    ($inst_name: ident, $($args: ident: $typedef: ty),* ) => {
        #[derive(Debug)]
        pub struct $inst_name {
            $(
                $args: $typedef,
            )*
        }

        impl $inst_name {
            #[allow(dead_code)]
            pub fn new($($args: $typedef),*) -> Self {
                $inst_name {
                    $(
                        $args: $args,
                    )*
                }
            }
        }

        impl RSIR for $inst_name {
            fn export(&self) -> String{
                String::from("")
            }

            fn import(&self, _ir: String){
                todo!("IR importing is under construction")
            }

        }
    };
}

// this function is to see the contents of inst! macro
#[allow(dead_code)]
fn expand() {
    inst!(TestInstruction, READ test: String, WRITE test2: String);
}

#[cfg(test)]
mod tests {
    use crate::{
        instructions::{ADD, BITWISE_AND, SET},
        types::RegId,
        RuleSetIR,
    };

    #[test]
    fn test_ruleset_ir_merging() {
        let mut parent_ir = RuleSetIR::new();
        // SET instruction
        let set_inst_0 = SET::new(RegId::Reg0, 0);
        let set_inst_1 = SET::new(RegId::Reg1, 0);
        // ADD instruction
        let add_inst = ADD::new(RegId::Reg0, RegId::Reg1, 1);
        parent_ir.add_instruction(Box::new(set_inst_0));
        parent_ir.add_instruction(Box::new(set_inst_1));
        parent_ir.add_instruction(Box::new(add_inst));
        let mut child_ir = RuleSetIR::new();
        let bit_wise_and_inst = BITWISE_AND::new(RegId::Reg0, RegId::Reg1, 0);
        child_ir.add_instruction(Box::new(bit_wise_and_inst));

        parent_ir.merge(&mut child_ir);

        assert_eq!(parent_ir.instructions.len(), 4);
    }
}
