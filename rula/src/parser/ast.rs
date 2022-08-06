use std::fmt::Debug;
use std::iter::Iterator;
use std::path::PathBuf;

/**
 * Top AST node starts from RuLa
*/
#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    RuLa(RuLa),
    Test, // Debug use
}

// This doesn't do much, but could be used for preprocessing or something later.
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

// In top level AST could have main program or EOI(EOF)
// Ignore is used for comment but can be removed I believe
#[derive(Debug, Clone, PartialEq)]
pub enum RuLaKind {
    Program(Program),
    Ignore, // ignore symbol such as comment
    Eoi,
}

// Second layer AST called program and store a set of statements
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

// Not program can only include a set of statements
#[derive(Debug, Clone, PartialEq)]
pub enum ProgramKind {
    Stmt(Stmt),
}

// Statement AST could be the top level AST in user writable for now
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
}

/**
 * Let statement composed of identifier and expression
 *
 *  `ident`: LitKind::IdentLit
 *  `expr`: StmtKind::Expr
 *
 */
#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub ident: Box<LitKind>, // ExprKind::Ident
    pub expr: Box<StmtKind>,
}

impl Let {
    // Constructor with kind check
    pub fn new(identifier: Ident, expr: Expr) -> Let {
        Let {
            ident: Box::new(LitKind::Ident(identifier)),
            expr: Box::new(StmtKind::Expr(expr)),
        }
    }

    pub fn place_holder() -> Let {
        Let {
            ident: Box::new(LitKind::PlaceHolder),
            expr: Box::new(StmtKind::PlaceHolder),
        }
    }

    pub fn add_ident(&mut self, identifier: Ident) {
        self.ident = Box::new(LitKind::Ident(identifier));
    }

    pub fn add_expr(&mut self, expr: Expr) {
        self.expr = Box::new(StmtKind::Expr(expr));
    }
}

/**
 * Expression structure can take ExprKind
 *
 *  `expr`: ExprKind
*/
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
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

/**
 * ExprKind can be
 * `Import` (import modules)
 * `If` (if expression)
 * `FnDef` (function definition)
 * `Lit` (literatures)
 * `Term` (number terms)
 * `PlaceHolder` This for initialization use. This should not be in the final AST
 * */
#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Import(Import),
    If(If),
    For(For),
    While(While),
    FnDef(FnDef),
    FnCall(FnCall),
    Comp(Comp),
    Array(Array), // [..]
    Lit(Lit),
    Term(f64),   // There could be better way. Leave this for now.
    PlaceHolder, // for initializing reason, but maybe better way?
}

/**
 * Import modules with path name
 *
 *  `path`: Box<PathKind> path to module
*/
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

/**
 * If expression (e.g.) if(a == b) {c;};
 *
 *  `block`: block expression in '(' ~ ')'
 *  `stmt`: statement in '{' ~ '}'
 *  `elif`: optional another if statement
 *  `else`: optional final actions
*/
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

// for_expr = { ^"for" ~ "(" ~ pattern ~")"~ "in" ~ expr ~ brace_stmt }
#[derive(Debug, Clone, PartialEq)]
pub struct For {
    pub pattern: Vec<Ident>,
    pub generator: Box<Expr>,
    pub stmt: Box<Stmt>,
}

impl For {
    pub fn new(idents: Vec<Ident>, expression: Expr, statement: Stmt) -> Self {
        For {
            pattern: idents,
            generator: Box::new(expression),
            stmt: Box::new(statement),
        }
    }
    pub fn place_holder() -> Self {
        For {
            pattern: vec![],
            generator: Box::new(Expr::place_holder()),
            stmt: Box::new(Stmt::place_holder()),
        }
    }
    pub fn add_ident(&mut self, ident: Ident) {
        self.pattern.push(ident);
    }
    pub fn add_expr(&mut self, expr: Expr) {
        self.generator = Box::new(expr);
    }
    pub fn add_stmt(&mut self, stmt: Stmt) {
        self.stmt = Box::new(stmt);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub block: Box<Expr>,
    pub stmt: Box<Stmt>,
}

impl While {
    pub fn new(block_expr: Expr, stmt: Stmt) -> Self {
        While {
            block: Box::new(block_expr),
            stmt: Box::new(stmt),
        }
    }
    pub fn place_holder() -> Self {
        While {
            block: Box::new(Expr::place_holder()),
            stmt: Box::new(Stmt::place_holder()),
        }
    }
    pub fn add_block(&mut self, block: Expr) {
        self.block = Box::new(block)
    }
    pub fn add_stmt(&mut self, stmt: Stmt) {
        self.stmt = Box::new(stmt)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDef {
    pub arguments: Vec<LitKind>, // LitKind::Ident
    pub stmt: Box<Stmt>,
}

impl FnDef {
    pub fn new(args: Vec<LitKind>, stmt: Stmt) -> FnDef {
        FnDef {
            arguments: args,
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
        self.arguments.push(LitKind::Ident(argument));
    }

    pub fn add_expr(&mut self, stmt: Stmt) {
        self.stmt = Box::new(stmt);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnCall {
    pub func_name: Box<Ident>,
}

impl FnCall {
    pub fn new(name: Ident) -> Self {
        FnCall {
            func_name: Box::new(name),
        }
    }

    pub fn place_holder() -> Self {
        FnCall {
            func_name: Box::new(Ident::place_holder()),
        }
    }

    pub fn add_name(&mut self, name: Ident) {
        self.func_name = Box::new(name);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Comp {
    lhs: Box<Expr>, // comparable expression (should be traited later)
    comp_op: Box<CompOpKind>,
    rhs: Box<Expr>,
}

impl Comp {
    pub fn new(lhs: Expr, comp_op: CompOpKind, rhs: Expr) -> Self {
        Comp {
            lhs: Box::new(lhs),
            comp_op: Box::new(comp_op),
            rhs: Box::new(rhs),
        }
    }
    pub fn place_holder() -> Self {
        Comp {
            lhs: Box::new(Expr::place_holder()),
            comp_op: Box::new(CompOpKind::PlaceHolder),
            rhs: Box::new(Expr::place_holder()),
        }
    }
    pub fn add_lhs(&mut self, lhs: Expr) {
        self.lhs = Box::new(lhs)
    }
    pub fn add_rhs(&mut self, rhs: Expr) {
        self.rhs = Box::new(rhs)
    }
    pub fn add_comp_op(&mut self, op: CompOpKind) {
        self.comp_op = Box::new(op)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompOpKind {
    Lt,  // `<`
    Gt,  // `>`
    LtE, // `<=`,
    GtE, // '>='
    Eq,  // `==`
    Nq,  // `!=`
    PlaceHolder,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    // pub type_hint: Box<TypeDef>,
    pub items: Vec<Lit>,
}

impl Array {
    pub fn new(item_vec: Vec<Lit>) -> Self {
        Array {
            // type_hint: Box::new(type_val),
            items: item_vec,
        }
    }
    pub fn place_holder() -> Self {
        Array {
            // type_hint: Box::new(TypeDef::PlaceHolder),
            items: vec![],
        }
    }
    // pub fn add_type_hind(typedef: TypeDef)

    pub fn add_item(&mut self, item: Lit) {
        self.items.push(item)
    }
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
    BooleanLit(bool),
    Ident(Ident),
    StringLit(StringLit),
    NumberLit(NumberLit),
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

#[derive(Debug, Clone, PartialEq)]
pub struct NumberLit {
    value: Box<String>, // At this point, number type cannot be determined
}

impl NumberLit {
    pub fn new(val: &str) -> Self {
        NumberLit {
            value: Box::new(String::from(val)),
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
    PlaceHolder,
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
