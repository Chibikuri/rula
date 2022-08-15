// This is entory point to generate code from AST
use super::error::*;
// use super::program::generate_program;
use proc_macro2::TokenStream;
use rula::parser::ast::*;
use std::collections::HashMap;
use std::marker::PhantomData;

pub type IResult<T> = std::result::Result<T, RuLaCompileError>;

// generate corresponding rust code from ast
pub fn generate(ast_tree: Vec<AstNode>) -> IResult<TokenStream> {
    // let mut rula_program = RuLaProgram::new();
    // for ast_node in ast_tree {
    //     match ast_node {
    //         AstNode::RuLa(rula) => rula_program = RuLaProgram::from(generate_rula(rula).unwrap()),
    //         AstNode::PlaceHolder => {
    //             return Err(RuLaCompileError::RuLaInitializationError(
    //                 InitializationError::new("at generate function"),
    //             ))
    //         }
    //     }
    // }
    // Ok(rula_program.gen_rust())
    Ok(quote!())
}

// trait generates rust code
pub trait GenRust {
    fn gen_rust(&self) -> RustProgram;
}

pub struct RuLaProgram {
    pub program: Box<Program>,
}

impl RuLaProgram {
    // pub fn new() -> Self {
    // RuLaProgram {
    //     program: Box::new(Program::place_holder()),
    //     // phantom: PhantomData
    // }
    // }
    pub fn from(rula_program: Box<Program>) -> Self {
        RuLaProgram {
            program: rula_program,
            // phantom: PhantomData
        }
    }
    pub fn get_str(&self) -> String {
        String::from("//Auto generated Rust code\n")
    }
}

// Gen rust called in nested way
impl GenRust for RuLaProgram {
    fn gen_rust(&self) -> RustProgram {
        RustProgram::new(None, RuLaProgram::get_str(self))
    }
}

fn generate_rula<T: GenRust + ?Sized>(rula: RuLa) -> IResult<Box<T>> {
    match *rula.rula {
        // RuLaKind::Program(program) => {
        //     // return Ok(generate_program(program).unwrap());
        // }
        // RuLaKind::Ignore => return Ok(Some()),
        // RuLaKind::Eoi => return Ok(Some(RuLaProgram::new())),
        RuLaKind::PlaceHolder => {
            panic!("Error: value not properly set")
        }
        _ => todo!(),
    }
}

// #[derive(Default)]
pub struct RProgram {
    pub program: Box<RuLaProgram>, // descendant　programs
    pub additional_comments: String,
}
impl RProgram {}

impl GenRust for Program {
    fn gen_rust(&self) -> RustProgram {
        RustProgram::new(None, String::from(""))
    }
}

#[derive(Default)]
pub struct RuLaEoi<T: GenRust + ?Sized> {
    pub phantom: PhantomData<T>,
}

impl<'a, T: GenRust + ?Sized> RuLaEoi<T> {
    pub fn new() -> Self {
        RuLaEoi {
            phantom: PhantomData,
        }
    }
}

impl<T: GenRust + ?Sized> GenRust for RuLaEoi<T> {
    fn gen_rust(&self) -> RustProgram {
        RustProgram::new(None, String::from("//End of Input"))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_generation() {
        let target_ast = vec![AstNode::RuLa(RuLa::place_holder())];
        // let rula_program = RuLaProgram::<dyn GenRust + ?Sized>::new();
        // let result = generate(target_ast).unwrap();
        // println!("Result{:#?}", result);
    }
}
