#[macro_use]
extern crate quote;
extern crate syn;

pub mod codegen {
    #[cfg(not(tarpaulin_include))]
    mod error;

    pub mod generator;

    use error::RuLaCompileError;
    pub type IResult<T> = std::result::Result<T, RuLaCompileError>;
}

pub mod rulep {
    pub mod action;
    pub mod condition;
    pub mod ruleset;
    pub mod ruleset_gen;

    #[cfg(not(tarpaulin_include))]
    mod error;
    use error::RuleSetGenerationError;
    pub type IResult<T> = std::result::Result<T, RuleSetGenerationError>;
}

pub mod network {
    pub mod qnic;
    pub mod qubit;
}

pub mod utils {}

#[cfg(test)]
mod tests {
    use rula::parser::ast::{AstNode, RuLa, RuLaKind};

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
