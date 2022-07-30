use std::fmt::Debug;
use std::iter::Iterator;
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Stmt(Stmt),
    Test, // Debug use
    Eoi,  // End of input
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub kind: Box<StmtKind>,
}

impl Stmt {
    pub fn new(stmt_kind: StmtKind) -> Stmt {
        Stmt {
            kind: Box::new(stmt_kind),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    Let,
    Expr(Expr),
    Test, // debug use
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    // What kind of expression
    pub expr: Box<ExprKind>,
}

impl Expr {
    pub fn new(exp_kind: ExprKind) -> Expr {
        Expr {
            expr: Box::new(exp_kind),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Import(Import),
    If(If),
    Fn(Fn),
    Test, // debug
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub path: Box<PathKind>,
}

impl Import {
    pub fn new(path: PathKind) -> Import {
        Import {
            path: Box::new(path),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub block: Box<Expr>,
    pub expr: Box<Expr>,
    pub elif: Option<Box<If>>,
    pub els: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fn {
    pub arguments: Box<Expr>,
    pub expr: Box<Expr>,
}

// impl IfKind{
//     pub fn new(cond: Expr, act: Expr) -> IfKind{
//         IfKind{condition: Box::new(cond), action: Box::new(act)}
//     }
// }

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
