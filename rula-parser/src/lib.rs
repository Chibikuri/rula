pub mod parser;
extern crate pest;
#[macro_use]
extern crate pest_derive;

#[derive(Parser)]
#[grammar = "parser/rula.pest"]
pub struct RuLaParser;

use parser::ast::AstNode;
use parser::error::RuLaError;
use parser::IResult;
use pest::Parser;

use crate::parser::build_ast_from_rula;

// Interface function to parse rula code
// Using IResult to propagate Syntax Error
// pest error can be caught inside the function because it's critical
pub fn parse(source: &str) -> IResult<Vec<AstNode>> {
    let mut ast = vec![];
    let pairs;
    match RuLaParser::parse(Rule::rula, source) {
        Ok(parsed) => pairs = parsed,
        Err(e) => {
            println!("Pest Error message {:?}", e);
            return Err(RuLaError::RuLaSyntaxError);
        }
    }

    for pair in pairs {
        match pair.as_rule() {
            Rule::rula => {
                ast.push(AstNode::RuLa(
                    build_ast_from_rula(pair.into_inner().next().unwrap()).unwrap(),
                ));
            }
            _ => {
                return Err(RuLaError::RuLaSyntaxError);
            }
        }
    }
    Ok(ast)
}

#[cfg(test)]
mod tests {
    use super::parse;
    #[test]
    fn test_non_rula_program() {
        // unknown input sequence
        let input = "####";
        assert!(parse(input).is_err());
    }
}
