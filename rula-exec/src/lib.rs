#[macro_use]
extern crate quote;
extern crate syn;

// Generator of ruleset
pub mod ruleset_gen {
    #[cfg(not(tarpaulin_include))]
    mod error;

    pub mod action;
    pub mod condition;
    pub mod conf_parser;
    pub mod factory;
    pub mod generator;
    pub mod ruleset;
    pub mod ruleset_generator;
    pub mod tracker;
    pub mod types;

    mod test_ruleset_generator;

    use error::RuleSetGenError;
    type IResult<T> = std::result::Result<T, RuleSetGenError>;
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
