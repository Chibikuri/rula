// This is entory point to generate code from AST
use super::error::RuLaCompileError;
use rula::parser::ast::*;
use std::error::Error;
use std::fs;
use std::io::Write;

pub type IResult<T> = std::result::Result<T, Box<dyn Error>>;

// generate corresponding rust code from ast
pub fn generate(ast_tree: Vec<AstNode>) -> RustProgram {
    let mut rust_program = RustProgram {
        program: String::from(""),
    };
    for ast_node in ast_tree {
        match ast_node {
            AstNode::RuLa(rula) => {
                rust_program = RustProgram::from(generate_rula(rula).unwrap())
                // println!("hello{:#?}", rula);
            }
            AstNode::PlaceHolder => {
                // return Box::mew(Err(RuLaCompileError::RuLaGenerationError))
                // panic!("Value not properly set");
            }
        }
    }
    rust_program
}

pub fn generate_file(program: RustProgram, file_name: String) {
    let mut file = fs::File::create(file_name).unwrap();
    file.write_all(program.program.as_bytes()).unwrap();
}

#[derive(Debug)]
pub struct RustProgram {
    pub program: String,
}

impl RustProgram {
    pub fn from(program: RuLaProgram) -> RustProgram {
        RustProgram {
            program: program.gen_rust(),
        }
    }
}

fn generate_rula(rula: RuLa) -> IResult<RuLaProgram> {
    match *rula.rula {
        RuLaKind::Program(program) => {
            println!("program");
        },
        RuLaKind::Ignore => {
            println!("comment")
        }
        RuLaKind::Eoi => {
            println!("end of input")
        }
        RuLaKind::PlaceHolder => {
            println!("Should be value error")
        }
        _ => unreachable!(),
    }
    let rula = RuLaProgram::new();
    Ok(rula)
}

// trait generates rust code
pub trait GenRust {
    fn gen_rust(&self) -> String;
}

#[derive(Debug)]
pub struct RuLaProgram {}
impl RuLaProgram {
    pub fn new() -> Self {
        RuLaProgram {}
    }
}

impl GenRust for RuLaProgram {
    fn gen_rust(&self) -> String {
        String::from("// RuLa program")
    }
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
