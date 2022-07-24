use std::fmt::{Debug, Error, Formatter};

pub enum Expr {
    Number(i32),
    Op(Box<Expr>, OpCode, Box<Expr>),
    Error,
}

#[derive(Copy, Clone)]
pub enum OpCode {
    Mul,
    Div,
    Add,
    Sub,
}

impl Debug for Expr {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::Expr::*;
        match *self {
            Number(n) => write!(fmt, "{:?}", n),
            Op(ref l, op, ref r) => write!(fmt, "({:?} {:?} {:?})", l, op, r),
            Error => write!(fmt, "error"),
        }
    }
}

// impl<'input> Debug for ExprSymbol<'input> {
//     fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
//         use self::ExprSymbol::*;
//         match *self {
//             NumSymbol(n) => write!(fmt, "{:?}", n),
//             Op(ref l, op, ref r) => write!(fmt, "({:?} {:?} {:?})", l, op, r),
//             Error => write!(fmt, "error"),
//         }
//     }
// }

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
