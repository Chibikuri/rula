#[allow(dead_code)]
pub mod instructions;
// pub mod ir_trait;

// use crate::ir_trait::IR;
// use crate::ir_trait::RSIR;
pub type IR = String;

// All the ruleset instructions needs to implement this trait RuleSet IR (RSIR).
// This trait implements `gen_ir` that returns a text format instruction for RuleSet
pub trait RSIR {
    fn gen_ir() -> IR;
}


#[macro_export]
macro_rules! inst {
    ($inst_name: ident, $($args: ident: $typedef: ty),* ) => {
            #[derive(Debug)]
            struct $inst_name{
                $(
                    $args: $typedef,
                )*
            }

            impl $inst_name{
                #[allow(dead_code)]
                pub fn new($($args: $typedef),*) -> Self{
                    $inst_name{
                        $(
                            $args: $args,
                        )*
                    }
                }
            }

            impl RSIR for $inst_name{
                fn gen_ir() -> IR {
                    "test".to_string()
                }
            }
        }
}

// this function is to see the contents of inst! macro
#[allow(dead_code)]
fn expand() {
    inst!(TestInstruction, test: String, test2: String);
}

#[cfg(test)]
mod macro_test {
    use super::*;
    #[test]
    fn macro_expansion() {
        inst!(TestInstruction, test: String);
    }
}
