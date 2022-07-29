pub mod parser;
extern crate pest;
#[macro_use]
extern crate pest_derive;

#[derive(Parser)]
#[grammar = "parser/rula.pest"]
pub struct RuLaParser;

use parser::ast::AstNode;
use parser::IResult;
use pest::Parser;

use crate::parser::build_ast_from_rula;

// Interface function to parse rula code
// Using IResult to propagate Syntax Error
// pest error can be caught inside the function because it's critical
pub fn parse(source: &str) -> IResult<Vec<AstNode>> {
    let mut ast = vec![];
    let pairs = RuLaParser::parse(Rule::rula, source).unwrap();

    for pair in pairs {
        match pair.as_rule() {
            Rule::rula => {
                ast.push(build_ast_from_rula(pair.into_inner().next().unwrap())?);
            }
            // Should have better error type and message here
            _ => panic!("RuLa program must be start with rula not: {:#?}", pair),
        }
    }
    Ok(ast)
}

#[cfg(test)]
mod tests {
    use super::parse;
    #[test]
    #[should_panic]
    fn test_non_rula_program() {
        // unknown input sequence
        let input = "####";
        let _ = parse(input);
    }
}
