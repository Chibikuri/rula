// This is entory point to generate code from AST
use super::error::RuLaCompileError;
use super::program::generate_program;
use rula::parser::ast::*;
use std::collections::HashMap;
use std::error::Error;
use std::hash::Hash;
use std::marker::PhantomData;

pub type IResult<T> = std::result::Result<T, Box<dyn Error>>;

// generate corresponding rust code from ast
pub fn generate(ast_tree: Vec<AstNode>) -> RustProgram {
    let mut rula_program = RuLaProgram::new();
    for ast_node in ast_tree {
        match ast_node {
            AstNode::RuLa(rula) => {
                rula_program = RuLaProgram::from(generate_rula(rula).unwrap())
            }
            AstNode::PlaceHolder => {
                // return Box::mew(Err(RuLaCompileError::RuLaGenerationError))
                // panic!("Value not properly set");
            }
        }
    }
    rula_program.gen_rust()
}

// trait generates rust code
pub trait GenRust {
    fn gen_rust(&self) -> RustProgram;
}

#[derive(Debug)]
pub struct RuLaProgram {
    pub program: Box<Option<RuLaProgram>>,
    // phantom: PhantomData<T>,
}

impl RuLaProgram {
    pub fn new() -> Self {
        RuLaProgram {
            program: Box::new(None),
            // phantom: PhantomData
        }
    }
    pub fn from(rula_program: Option<RuLaProgram>) -> Self {
        RuLaProgram {
            program: Box::new(rula_program),
            // phantom: PhantomData
        }
    }
    pub fn get_str(&self) -> String{
        String::from("//Auto generated Rust code\n")
    }
}

impl GenRust for RuLaProgram {
    fn gen_rust(&self) -> RustProgram {
        let prog = match &*self.program{
            Some(rula_program) => { rula_program.gen_rust() }
            None => { RustProgram::from(None, "") }
        };
        RustProgram::new(
            None, 
            RuLaProgram::get_str(self) + prog.program.as_str(),
        )
    }
}

// Do we need phantom here?
#[derive(Debug)]
pub struct RustProgram {
    pub metadata: Option<HashMap<String, String>>,
    pub program: String,
}

impl RustProgram {
    pub fn new(meta_data: Option<HashMap<String, String>>, program_str: String) -> Self {
        RustProgram {
            metadata: meta_data,
            program: program_str,
        }
    }

    pub fn from(meta_data: Option<HashMap<String, String>>, program_str: &str) -> Self {
        RustProgram {
            metadata: None,
            program: String::from(program_str),
        }
    }
}

fn generate_rula(rula: RuLa) -> IResult<Option<RuLaProgram>> {
    match *rula.rula {
        RuLaKind::Program(program) => {
            generate_program(program).unwrap();
        }
        RuLaKind::Ignore => {
            println!("comment")
        }
        RuLaKind::Eoi => {
            println!("end of input")
        }
        RuLaKind::PlaceHolder => {
            println!("Should be value error")
        } // _ => unreachable!(),
    }
    let rula = RuLaProgram::new();
    Ok(Some(rula))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_generation() {
        let target_ast = vec![AstNode::RuLa(RuLa::place_holder())];
        let result = generate(target_ast);
        println!("Result{:#?}", result);
        // generate_file(result, String::from("./test.rs"))
    }
}
