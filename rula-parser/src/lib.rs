pub mod parser;
extern crate pest;
#[macro_use]
extern crate pest_derive;

#[derive(Parser)]
#[grammar = "parser/rula.pest"]
pub struct RuLaParser;

use std::path::PathBuf;

use parser::ast::AstNode;
use parser::error::RuLaError;
use parser::IResult;
use pest::Parser;

use crate::parser::build_ast_from_rula;

// Interface function to parse rula code
// Using IResult to propagate Syntax Error
// pest error can be caught inside the function because it's critical
pub fn parse(source: &str, file_path: &PathBuf) -> IResult<AstNode> {
    // register file path to the file
    // convert file path to the absolute path
    let mut ast = AstNode::PlaceHolder;
    let pairs = match RuLaParser::parse(Rule::rula, source) {
        Ok(parsed) => parsed,
        Err(e) => {
            println!("Pest Error message {:?}", e);
            return Err(RuLaError::RuLaSyntaxError);
        }
    };

    for pair in pairs {
        match pair.as_rule() {
            Rule::rula => {
                ast = AstNode::RuLa(
                    build_ast_from_rula(pair.into_inner().next().unwrap(), file_path).unwrap(),
                );
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
    use std::path::PathBuf;

    use super::parse;
    #[test]
    fn test_non_rula_program() {
        // unknown input sequence
        let input = "####";
        assert!(parse(input, &PathBuf::from("/")).is_err());
    }
}
