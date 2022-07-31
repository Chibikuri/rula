use std::fmt::Debug;
use std::iter::Iterator;
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Stmt(Stmt),
    Ignore, // ignore symbol such as comment
    Test,   // Debug use
    Eoi,    // End of input
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
    pub fn place_holder() -> Stmt {
        Stmt {
            kind: Box::new(StmtKind::PlaceHolder),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    Let(Let),
    Expr(Expr),
    PlaceHolder, // For initialization use
    Test,        // debug use
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub ident: Box<ExprKind>, // ExprKind::Ident
    pub expr: Box<StmtKind>,  // StmtKind::Expr
}

impl Let {
    pub fn new(identity: ExprKind, expr: StmtKind) -> Let {
        Let {
            ident: Box::new(identity),
            expr: Box::new(expr),
        }
    }
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

    pub fn place_holder() -> Expr {
        Expr {
            expr: Box::new(ExprKind::PlaceHolder),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Import(Import),
    If(If),
    Fn(Fn),
    StringLit(StringLit),
    Ident(Ident),
    PlaceHolder, // for initializing reason, but maybe better way?
    Test,        // debug
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub name: Box<String>,
}

impl Ident {
    pub fn new(name: &str) -> Ident {
        Ident {
            name: Box::new(String::from(name)),
        }
    }
    // Do we have better way?
    pub fn place_holder() -> Ident {
        Ident {
            name: Box::new(String::from("")),
        }
    }
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
    pub block: Box<StmtKind>, // StmtKind::Expr
    pub stmt: Box<Stmt>,
    pub elif: Box<Option<If>>,
    pub els: Box<Option<Stmt>>,
}

impl If {
    pub fn new(block: StmtKind, stmt: Stmt, elif: Option<If>, els: Option<Stmt>) -> If {
        If {
            block: Box::new(block),
            stmt: Box::new(stmt),
            elif: Box::new(elif),
            els: Box::new(els),
        }
    }

    pub fn place_holder() -> If {
        If {
            block: Box::new(StmtKind::PlaceHolder),
            stmt: Box::new(Stmt::place_holder()),
            elif: Box::new(None),
            els: Box::new(None),
        }
    }

    pub fn add_block(&mut self, block: StmtKind) {
        self.block = Box::new(block);
    }

    pub fn add_stmt(&mut self, stmt: Stmt){
        self.stmt = Box::new(stmt);
    }

    pub fn add_else(&mut self, els: Stmt){
        self.els = Box::new(Some(els));
    }

    pub fn check(&self) {
        if *self.block == StmtKind::PlaceHolder {
            // Maybe just error returning
            panic!("No block expressio set!");
        }
        if *self.stmt == Stmt::place_holder(){
            panic!("No statement found!");
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fn {
    pub arguments: Box<Expr>,
    pub expr: Box<Expr>,
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
pub struct StringLit {
    string: Box<String>,
}

impl StringLit {
    pub fn new(strs: &str) -> StringLit {
        StringLit {
            string: Box::new(String::from(strs)),
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
