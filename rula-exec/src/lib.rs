#[macro_use]
extern crate quote;
extern crate syn;

pub mod generator{
    pub mod generator;
    pub mod symbol_table;
    pub mod scope;
    pub mod types;

    #[cfg(not(tarpaulin_include))]
    mod error;

    use error::CompileError;
    type IResult<T> = std::result::Result<T, CompileError>;
}

// Generator of ruleset
#[deprecated]
pub mod ruleset_gen {
    #[cfg(not(tarpaulin_include))]
    mod error;

    pub mod action;
    pub mod condition;
    pub mod conf_parser;
    pub mod factory;
    pub mod ruleset;
    // ruleset_generator generates RuleSet in json format
    pub mod ruleset_generator;
    pub mod tracker;
    pub mod types;

    mod test_ruleset_generator;

    use error::InternalError;
    use error::RuleSetGenError;
    type IResult<T> = std::result::Result<T, RuleSetGenError>;
    type InternalResult<T> = std::result::Result<T, InternalError>;
}

#[cfg(test)]
mod tests {
    use rula_parser::parser::ast::{AstNode, RuLa, RuLaKind};

    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }

    #[test]
    fn piece_of_ast() {
        let ast = AstNode::RuLa(RuLa::new(RuLaKind::Eoi));
        let ast2 = AstNode::RuLa(RuLa::new(RuLaKind::Eoi));
        assert_eq!(ast, ast2);
    }
}
