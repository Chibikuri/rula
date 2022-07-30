use std::fmt::Debug;
use std::iter::{IntoIterator, Iterator};
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Integer(i32),
    Float(f64),
    Import(PathKind),
    If {
        expression: Box<Expr>,
    },
    Term {
        lhs: Box<AstNode>,
        op: Box<OpCode>,
        rhs: Box<AstNode>,
    },
    Else,
    Test,
    Eoi, // End of input
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathKind {
    paths: Vec<PathBuf>,
    index: u32, // for iterator (should have better way)
}

impl PathKind {
    pub fn new() -> PathKind {
        PathKind {
            paths: vec![],
            index: 0,
        }
    }

    pub fn from(path_vec: Vec<PathBuf>) -> PathKind {
        PathKind {
            paths: path_vec,
            index: 0,
        }
    }

    pub fn add(&mut self, path: PathBuf) {
        self.paths.push(path);
    }

    pub fn size_hint(&self) -> usize {
        self.paths.len()
    }
}

impl Iterator for PathKind {
    type Item = PathBuf;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index > self.paths.len() as u32 {
            return None;
        } else {
            let curr = self.index;
            self.index = self.index + 1;
            return Some(self.paths[curr as usize].clone());
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr;
// Number(i32),
// Op(Box<Expr>, OpCode, Box<Expr>),
// Error

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum OpCode {
    Mul,
    Div,
    Add,
    Sub,
}

// impl Debug for OpCode {
//     fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
//         use self::OpCode::*;
//         match *self {
//             Mul => write!(fmt, "*"),
//             Div => write!(fmt, "/"),
//             Add => write!(fmt, "+"),
//             Sub => write!(fmt, "-"),
//         }
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(test)]
    mod path_kind_test {
        use super::*;

        #[test]
        fn test_path_kind_new() {
            let path = PathKind::new();
            assert_eq!(path.paths.len(), 0);
        }

        #[test]
        fn test_path_kind_from() {
            let expected: Vec<PathBuf> = vec![
                ["hello", "world"].iter().collect(),
                ["hello", "there"].iter().collect(),
            ];
            let mut path_kind = PathKind::from(expected.clone());
            assert_eq!(path_kind.paths.len(), 2);
            assert_eq!(path_kind.next().unwrap(), expected[0]);
            assert_eq!(path_kind.next().unwrap(), expected[1])
        }

        #[test]
        fn test_path_kind_add() {
            let mut path_kind = PathKind::new();
            let expected: Vec<PathBuf> = vec![
                ["hello", "world"].iter().collect(),
                ["hello", "there"].iter().collect(),
            ];
            assert_eq!(path_kind.size_hint(), 0);
            let mut count = 0;
            for path in expected.clone() {
                path_kind.add(path);
                count = count + 1;
                assert_eq!(path_kind.size_hint(), count);
            }
            assert_eq!(path_kind.size_hint(), 2);
            assert_eq!(path_kind.next().unwrap(), expected[0]);
            assert_eq!(path_kind.next().unwrap(), expected[1]);
        }
    }
}
