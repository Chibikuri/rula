use super::generator::*;
use rula::parser::ast::*;

pub fn generate_program(program: Program) -> IResult<Option<RuLaProgram>> {
    match *program.kind {
        ProgramKind::Stmt(stmt) => {
            println!("{:#?}", stmt);
        }
    }
    Ok(None)
}
