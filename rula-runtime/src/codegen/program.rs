use super::generator::*;
use rula::parser::ast::*;

pub fn generate_program<T: GenRust + ?Sized>(program: Program) -> IResult<Box<T>> {
    match *program.kind {
        ProgramKind::Stmt(stmt) => {
            println!("{:#?}", stmt);
        }
    }
    Ok(program)
}
