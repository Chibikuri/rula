pub mod parser;
extern crate pest;
#[macro_use]
extern crate pest_derive;

#[derive(Parser)]
#[grammar = "parser/rula.pest"]
pub struct RuLaParser;

use parser::ast::AstNode;
use parser::error::IResult;
use pest::Parser;

use crate::parser::build_ast_from_program;

// Interface function to parse rula code
// Using IResult to propagate Syntax Error
// pest error can be caught inside the function because it's critical
pub fn parse(source: &str) -> IResult<Vec<AstNode>> {
    let mut ast = vec![];
    let pairs = RuLaParser::parse(Rule::program, source).unwrap();

    // println!("{:#?}", pairs);
    for pair in pairs {
        match pair.as_rule() {
            Rule::program => {
                ast.push(build_ast_from_program(pair.into_inner().next().unwrap())?);
            }
            _ => {
                panic!("Top AST must start program not {:?}", pair)
            }
        }
    }
    Ok(ast)
}
