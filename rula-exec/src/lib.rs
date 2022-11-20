#[macro_use]
extern crate quote;
extern crate syn;

pub mod codegen {
    #[cfg(not(tarpaulin_include))]
    mod error;

    pub mod default_token;
    pub mod generator;
    pub mod identifier;
    pub mod rule_meta;
    pub mod ruleset_generator;

    #[cfg(test)]
    pub mod test_generator;

    use error::RuLaCompileError;
    pub type IResult<T> = std::result::Result<T, RuLaCompileError>;
}

pub mod rulep {
    pub mod action;
    pub mod condition;
    pub mod ruleset;

    #[cfg(not(tarpaulin_include))]
    mod error;
    use error::RuleSetGenerationError;
    pub type IResult<T> = std::result::Result<T, RuleSetGenerationError>;
}

pub mod wrapper {
    pub mod qnic_wrapper;
    pub mod qubit_wrapper;
}

pub mod utils {}

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
