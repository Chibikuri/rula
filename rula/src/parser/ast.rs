use std::fmt::{Debug, Error, Formatter};

pub enum Expr {
    Number(i32),
    Op(Box<Expr>, OpCode, Box<Expr>),
    Error,
}

pub enum ExprSymbol<'input> {
    NumSymbol(&'input str),
    Op(Box<ExprSymbol<'input>>, OpCode, Box<ExprSymbol<'input>>),
    Error,
}

#[derive(PartialEq, Eq, Clone)]
pub enum OpCode {
    Mul,
    Div,
    Add,
    Sub,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Integer(i32),
    Float(f64),
    Term {
        lhs: Box<AstNode>,
        op: OpCode,
        rhs: Box<AstNode>,
    },
    If,
    Else,
    Test,
}
impl Debug for Expr {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::Expr::*;
        match self {
            Number(n) => write!(fmt, "{:?}", n),
            Op(ref l, op, ref r) => write!(fmt, "({:?} {:?} {:?})", l, op, r),
            Error => write!(fmt, "error"),
        }
    }
}

impl Debug for OpCode {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::OpCode::*;
        match *self {
            Mul => write!(fmt, "*"),
            Div => write!(fmt, "/"),
            Add => write!(fmt, "+"),
            Sub => write!(fmt, "-"),
        }
    }
}
