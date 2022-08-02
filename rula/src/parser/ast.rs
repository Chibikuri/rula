use std::fmt::Debug;
use std::iter::Iterator;
use std::path::PathBuf;

// top ast
#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    RuLa(RuLa),
    Test, // Debug use
}

#[derive(Debug, Clone, PartialEq)]
pub struct RuLa {
    rula: Box<RuLaKind>,
}

impl RuLa {
    // This way could be updated when we have more than two RuLaKind
    pub fn new(rula: RuLaKind) -> RuLa {
        RuLa {
            rula: Box::new(rula),
        }
    }
    pub fn ignore() -> RuLa {
        RuLa {
            rula: Box::new(RuLaKind::Ignore),
        }
    }
    pub fn aoi() -> RuLa {
        RuLa {
            rula: Box::new(RuLaKind::Eoi),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RuLaKind {
    Program(Program),
    Ignore, // ignore symbol such as comment
    Eoi,
}

// ast -> program
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub kind: Box<ProgramKind>, // Program Kind expr
}

impl Program {
    pub fn new(program: ProgramKind) -> Program {
        return Program {
            kind: Box::new(program),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ProgramKind {
    Stmt(Stmt),
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
    pub ident: Box<ExprKind>,
    pub expr: Box<StmtKind>,
}

impl Let {
    pub fn new(identifier: Ident, expr: Expr) -> Let {
        Let {
            ident: Box::new(ExprKind::Ident(identifier)),
            expr: Box::new(StmtKind::Expr(expr)),
        }
    }
    pub fn place_holder() -> Let {
        Let {
            ident: Box::new(ExprKind::PlaceHolder),
            expr: Box::new(StmtKind::PlaceHolder),
        }
    }
    pub fn add_ident(&mut self, identifier: Ident) {
        self.ident = Box::new(ExprKind::Ident(identifier));
    }

    pub fn add_expr(&mut self, expr: Expr) {
        self.expr = Box::new(StmtKind::Expr(expr));
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
    FnDef(FnDef),
    Lit(Lit),
    Ident(Ident),
    Term(i32),   // There could be better way. Leave this for now.
    PlaceHolder, // for initializing reason, but maybe better way?
    Test,        // debug
}

pub enum NumberKind {
    Integer(i64), // This could be converted to i32 later
    Float(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum OpKind {
    Plus,
    Minus,
    Asterisk,
    Slash,
    PlaceHolder,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lit {
    pub kind: Box<LitKind>,
}

impl Lit {
    pub fn new(kind: LitKind) -> Lit {
        Lit {
            kind: Box::new(kind),
        }
    }
    pub fn place_holder() -> Lit {
        Lit {
            kind: Box::new(LitKind::PlaceHolder),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LitKind {
    StringLit(StringLit),
    IntegerLit(IntegerLit),
    PlaceHolder,
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

// Should be able to integrate to 64
#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLit {
    // At this point, this is just string, but converted to integer
    integer: Box<String>,
}

impl IntegerLit {
    pub fn new(integer: &str) -> IntegerLit {
        IntegerLit {
            integer: Box::new(String::from(integer)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub name: Box<String>,
    pub type_hint: Box<Option<TypeDef>>,
}

impl Ident {
    pub fn new(name: &str, type_def: Option<TypeDef>) -> Ident {
        Ident {
            name: Box::new(String::from(name)),
            type_hint: Box::new(type_def),
        }
    }
    // Do we have better way?
    pub fn place_holder() -> Ident {
        Ident {
            name: Box::new(String::from("")),
            type_hint: Box::new(None),
        }
    }
    pub fn add_name(&mut self, name: &str) {
        self.name = Box::new(String::from(name));
    }

    pub fn add_type_hint(&mut self, type_hint: Option<TypeDef>) {
        self.type_hint = Box::new(type_hint);
    }
    pub fn check(&self) {
        if *self.name == String::from("") {
            panic!("Identify name not set!")
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDef {
    Integer32,
    Integer64,
    UnsignedInteger32,
    UnsignedInteger64,
    Float32,
    Float64,
    Complex64,
    Complex128,
    Boolean,
    Str,
    Qubit,
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
    pub block: Box<Expr>, // StmtKind::Expr
    pub stmt: Box<Stmt>,
    pub elif: Vec<Option<If>>, // Vec [ExprKind::If] (If ~ else ~ else is error in grammar level)
    pub els: Box<Option<Stmt>>,
}

impl If {
    pub fn new(block: Expr, stmt: Stmt, elif: Option<If>, els: Option<Stmt>) -> If {
        If {
            // enum type validation
            block: Box::new(block),
            stmt: Box::new(stmt),
            elif: vec![elif],
            els: Box::new(els),
        }
    }

    pub fn place_holder() -> If {
        If {
            block: Box::new(Expr::place_holder()),
            stmt: Box::new(Stmt::place_holder()),
            elif: vec![None],
            els: Box::new(None),
        }
    }

    pub fn add_block(&mut self, block: Expr) {
        self.block = Box::new(block);
    }

    pub fn add_stmt(&mut self, stmt: Stmt) {
        self.stmt = Box::new(stmt);
    }

    pub fn add_elif(&mut self, elif: If) {
        // Um, better way?
        if self.elif[0] == None {
            self.elif.pop();
        }
        self.elif.push(Some(elif));
    }

    pub fn add_else(&mut self, els: Stmt) {
        self.els = Box::new(Some(els));
    }

    pub fn check(&self) {
        if *self.block == Expr::place_holder() {
            // Maybe just error returning
            panic!("No block expressio set!");
        }
        if *self.stmt == Stmt::place_holder() {
            panic!("No statement found!");
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDef {
    pub arguments: Vec<Ident>,
    pub stmt: Box<Stmt>,
}

impl FnDef {
    pub fn new(arguments: Vec<Ident>, stmt: Stmt) -> FnDef {
        FnDef {
            arguments: arguments,
            stmt: Box::new(stmt),
        }
    }
    pub fn place_holder() -> FnDef {
        FnDef {
            arguments: vec![],
            stmt: Box::new(Stmt::place_holder()),
        }
    }

    pub fn add_arg(&mut self, argument: Ident) {
        self.arguments.push(argument);
    }

    pub fn add_expr(&mut self, stmt: Stmt) {
        self.stmt = Box::new(stmt);
    }
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
