use rula::parser::ast::*;
use super::generator::*;

pub fn generate_program(program: Program) -> IResult<Option<RuLaProgram>>{
    match *program.kind{
        ProgramKind::Stmt(stmt) => {
            println!("{:#?}", stmt);
        }
    }
    Ok(None)
}