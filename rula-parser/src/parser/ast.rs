use std::fmt::Debug;
use std::iter::Iterator;
use std::path::PathBuf;

/**
 * Top AST node starts from RuLa
*/
#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    RuLa(RuLa),
    PlaceHolder, // Debug use
}

// This doesn't do much, but could be used for preprocessing or something later.
#[derive(Debug, Clone, PartialEq)]
pub struct RuLa {
    pub rula: Box<RuLaKind>,
}

impl RuLa {
    // This way could be updated when we have more than two RuLaKind
    pub fn new(rula: RuLaKind) -> Self {
        RuLa {
            rula: Box::new(rula),
        }
    }

    pub fn place_holder() -> Self {
        RuLa {
            rula: Box::new(RuLaKind::PlaceHolder),
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
    PlaceHolder,
    Ignore, // ignore symbol such as comment
    Eoi,
}

// Second layer AST called program and store a set of statements
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub programs: Vec<ProgramKind>, // Program Kind expr
}

impl Program {
    pub fn new(programs: Vec<ProgramKind>) -> Self {
        Program { programs: programs }
    }

    pub fn place_holder() -> Self {
        Program { programs: vec![] }
    }

    pub fn add_program(&mut self, program: ProgramKind) {
        self.programs.push(program);
    }
}

// Not program can only include a set of statements
#[derive(Debug, Clone, PartialEq)]
pub enum ProgramKind {
    Stmt(Stmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Interface {
    pub interface: Vec<Ident>,
    pub group_name: Box<Option<Ident>>,
}

impl Interface {
    pub fn new(interfaces: Vec<Ident>, group_name: Option<Ident>) -> Self {
        Interface {
            interface: interfaces,
            group_name: Box::new(group_name),
        }
    }
    pub fn place_holder() -> Self {
        Interface {
            interface: vec![],
            group_name: Box::new(None),
        }
    }
    pub fn add_interface(&mut self, interface: Ident) {
        self.interface.push(interface);
    }
    pub fn add_name(&mut self, name: Option<Ident>) {
        self.group_name = Box::new(name);
    }
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
    Interface(Interface),
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
    pub ident: Box<Ident>, // ExprKind::Ident
    pub expr: Box<Expr>,
}

impl Let {
    // Constructor with kind check
    pub fn new(identifier: Ident, expr: Expr) -> Let {
        Let {
            ident: Box::new(identifier),
            expr: Box::new(expr),
        }
    }

    pub fn place_holder() -> Let {
        Let {
            ident: Box::new(Ident::place_holder()),
            expr: Box::new(Expr::place_holder()),
        }
    }

    pub fn add_ident(&mut self, identifier: Ident) {
        self.ident = Box::new(identifier);
    }

    pub fn add_expr(&mut self, expr: Expr) {
        self.expr = Box::new(expr);
    }
}

/**
 * Expression structure can take ExprKind
 *
 *  `expr`: ExprKind
*/
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: Box<ExprKind>,
}

impl Expr {
    pub fn new(exp_kind: ExprKind) -> Expr {
        Expr {
            kind: Box::new(exp_kind),
        }
    }

    pub fn place_holder() -> Expr {
        Expr {
            kind: Box::new(ExprKind::PlaceHolder),
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
    Struct(Struct),
    Return(Return),
    Match(Match),
    Comp(Comp),
    RuleSetExpr(RuleSetExpr),
    RuleExpr(RuleExpr),
    CondExpr(CondExpr),
    ActExpr(ActExpr),
    VariableCallExpr(VariableCallExpr),
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
    pub paths: Vec<PathBuf>,
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

    pub fn convert_to_ident(&self) -> Vec<Ident> {
        let mut ident_vec = vec![];
        for p in self.paths.iter() {
            ident_vec.push(Ident::new(
                p.clone().into_os_string().to_str().unwrap(),
                None,
            ))
        }
        ident_vec
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
    pub arguments: Vec<Ident>, // LitKind::Ident
    pub stmt: Box<Stmt>,
}

impl FnDef {
    pub fn new(args: Vec<Ident>, stmt: Stmt) -> FnDef {
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
        self.arguments.push(argument);
    }

    pub fn add_expr(&mut self, stmt: Stmt) {
        self.stmt = Box::new(stmt);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnCall {
    pub func_name: Box<Ident>,
    pub arguments: Vec<Expr>,
}

impl FnCall {
    pub fn new(name: Ident, arguments: Vec<Expr>) -> Self {
        FnCall {
            func_name: Box::new(name),
            arguments: arguments,
        }
    }

    pub fn place_holder() -> Self {
        FnCall {
            func_name: Box::new(Ident::place_holder()),
            arguments: vec![],
        }
    }

    pub fn add_name(&mut self, name: Ident) {
        self.func_name = Box::new(name);
    }
    pub fn add_argument(&mut self, arg: Expr) {
        self.arguments.push(arg);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: Box<Ident>,
    pub items: Vec<Ident>,
}

impl Struct {
    pub fn new(name: Ident, item_vec: Vec<Ident>) -> Self {
        Struct {
            name: Box::new(name),
            items: item_vec,
        }
    }
    pub fn place_holder() -> Self {
        Struct {
            name: Box::new(Ident::place_holder()),
            items: vec![],
        }
    }

    pub fn add_name(&mut self, name: Ident) {
        self.name = Box::new(name)
    }
    pub fn add_item(&mut self, item: Ident) {
        self.items.push(item)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub target: Box<Expr>,
}

impl Return {
    pub fn new(return_target: Expr) -> Self {
        Return {
            target: Box::new(return_target),
        }
    }
    pub fn place_holder() -> Self {
        Return {
            target: Box::new(Expr::place_holder()),
        }
    }
    pub fn add_target(&mut self, target: Expr) {
        self.target = Box::new(target);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Match {
    pub temp_val: Box<Option<Ident>>,
    pub expr: Box<Expr>,
    pub match_arms: Vec<MatchArm>,
    pub finally: Box<Option<MatchAction>>,
}

impl Match {
    pub fn new(
        temp_val: Option<Ident>,
        expr: Expr,
        match_arms: Vec<MatchArm>,
        finally: Option<MatchAction>,
    ) -> Self {
        Match {
            temp_val: Box::new(temp_val),
            expr: Box::new(expr),
            match_arms: match_arms,
            finally: Box::new(finally),
        }
    }

    pub fn place_holder() -> Self {
        Match {
            temp_val: Box::new(None),
            expr: Box::new(Expr::place_holder()),
            match_arms: vec![],
            finally: Box::new(None),
        }
    }
    pub fn add_temp_val(&mut self, temp_val: Option<Ident>) {
        self.temp_val = Box::new(temp_val);
    }
    pub fn add_expr(&mut self, expr: Expr) {
        self.expr = Box::new(expr);
    }
    pub fn add_match_arm(&mut self, match_arm: MatchArm) {
        self.match_arms.push(match_arm);
    }
    pub fn add_finally(&mut self, finally: Option<MatchAction>) {
        self.finally = Box::new(finally);
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub condition: Box<MatchCondition>,
    pub action: Box<MatchAction>,
}

impl MatchArm {
    pub fn new(match_condition: MatchCondition, match_action: MatchAction) -> Self {
        MatchArm {
            condition: Box::new(match_condition),
            action: Box::new(match_action),
        }
    }
    pub fn place_holder() -> Self {
        MatchArm {
            condition: Box::new(MatchCondition::place_holder()),
            action: Box::new(MatchAction::place_holder()),
        }
    }
    pub fn add_condition(&mut self, match_condition: MatchCondition) {
        self.condition = Box::new(match_condition);
    }
    pub fn add_action(&mut self, match_action: MatchAction) {
        self.action = Box::new(match_action);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchCondition {
    pub satisfiable: Box<Expr>,
}

impl MatchCondition {
    pub fn new(satisfiable: Expr) -> Self {
        MatchCondition {
            satisfiable: Box::new(satisfiable),
        }
    }
    pub fn place_holder() -> Self {
        MatchCondition {
            satisfiable: Box::new(Expr::place_holder()),
        }
    }
    pub fn add_satisfiable(&mut self, satisfiable: Expr) {
        self.satisfiable = Box::new(satisfiable);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchAction {
    pub actionable: Box<Expr>,
}

impl MatchAction {
    pub fn new(actionable: Expr) -> Self {
        MatchAction {
            actionable: Box::new(actionable),
        }
    }
    pub fn place_holder() -> Self {
        MatchAction {
            actionable: Box::new(Expr::place_holder()),
        }
    }
    pub fn add_actionable(&mut self, actionable: Expr) {
        self.actionable = Box::new(actionable);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Comp {
    pub lhs: Box<Expr>, // comparable expression (should be traited later)
    pub comp_op: Box<CompOpKind>,
    pub rhs: Box<Expr>,
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
pub struct RuleSetExpr {
    pub name: Box<Ident>,
    pub config: Box<Option<Ident>>,
    pub default: Box<Option<FnCall>>,
    pub rules: Vec<Stmt>, // This could be the vector of Rules
}

impl RuleSetExpr {
    pub fn new(
        name: Ident,
        config: Option<Ident>,
        default: Option<FnCall>,
        rules: Vec<Stmt>,
    ) -> Self {
        RuleSetExpr {
            name: Box::new(name),
            config: Box::new(config),
            default: Box::new(default),
            rules: rules,
        }
    }
    pub fn place_holder() -> Self {
        RuleSetExpr {
            name: Box::new(Ident::place_holder()),
            config: Box::new(None),
            default: Box::new(None),
            rules: vec![],
        }
    }
    pub fn add_name(&mut self, name: Ident) {
        self.name = Box::new(name);
    }
    pub fn add_default(&mut self, default: Option<FnCall>) {
        self.default = Box::new(default);
    }
    pub fn add_rules(&mut self, rules: Stmt) {
        self.rules.push(rules);
    }
}

// rule_expr = {^"rule" ~ angle_expr ~ arguments ~ brace_stmt}
#[derive(Debug, Clone, PartialEq)]
pub struct RuleExpr {
    pub name: Box<Ident>,
    pub interface: Vec<Ident>,
    pub args: Vec<Ident>,
    pub rule_content: Box<RuleContentExpr>,
}

impl RuleExpr {
    pub fn new(
        name: Ident,
        interfaces: Vec<Ident>,
        arg_vec: Vec<Ident>,
        rule_content: RuleContentExpr,
    ) -> Self {
        RuleExpr {
            name: Box::new(name),
            interface: interfaces,
            args: arg_vec,
            rule_content: Box::new(rule_content),
        }
    }
    pub fn place_holder() -> Self {
        RuleExpr {
            name: Box::new(Ident::place_holder()),
            interface: vec![],
            args: vec![],
            rule_content: Box::new(RuleContentExpr::place_holder()),
        }
    }
    pub fn add_name(&mut self, name: Ident) {
        self.name = Box::new(name);
    }
    pub fn add_interface(&mut self, interface_name: Ident) {
        self.interface.push(interface_name);
    }
    pub fn add_arg(&mut self, arg: Ident) {
        self.args.push(arg);
    }
    pub fn add_rule_content(&mut self, rule_content: RuleContentExpr) {
        self.rule_content = Box::new(rule_content)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RuleContentExpr {
    pub monitor_expr: Box<Option<WatchExpr>>,
    pub condition_expr: Box<CondExpr>,
    pub action_expr: Box<ActExpr>,
    pub post_processing: Vec<Stmt>,
}

impl RuleContentExpr {
    pub fn new(
        monitor_expr: Option<WatchExpr>,
        condition_expr: CondExpr,
        action_expr: ActExpr,
        post_processing: Vec<Stmt>,
    ) -> Self {
        RuleContentExpr {
            monitor_expr: Box::new(monitor_expr),
            condition_expr: Box::new(condition_expr),
            action_expr: Box::new(action_expr),
            post_processing: post_processing,
        }
    }
    pub fn place_holder() -> Self {
        RuleContentExpr {
            monitor_expr: Box::new(None),
            condition_expr: Box::new(CondExpr::place_holder()),
            action_expr: Box::new(ActExpr::place_holder()),
            post_processing: vec![],
        }
    }
    pub fn add_monitor_expr(&mut self, monitor_expr: Option<WatchExpr>) {
        self.monitor_expr = Box::new(monitor_expr);
    }
    pub fn add_condition_expr(&mut self, condition_expr: CondExpr) {
        self.condition_expr = Box::new(condition_expr);
    }
    pub fn add_action_expr(&mut self, action_expr: ActExpr) {
        self.action_expr = Box::new(action_expr);
    }
    pub fn add_post_stmt(&mut self, stmt: Stmt) {
        self.post_processing.push(stmt);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WatchExpr {
    pub monitored_values: Vec<Let>,
}

impl WatchExpr {
    pub fn new(monitored_values: Vec<Let>) -> Self {
        WatchExpr {
            monitored_values: monitored_values,
        }
    }
    pub fn place_holder() -> Self {
        WatchExpr {
            monitored_values: vec![],
        }
    }
    pub fn add_monitor_value(&mut self, monitor_value: Let) {
        self.monitored_values.push(monitor_value);
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct CondExpr {
    pub name: Box<Option<Ident>>,
    pub clauses: Vec<Awaitable>,
}

impl CondExpr {
    pub fn new(name: Option<Ident>, awaitables: Vec<Awaitable>) -> Self {
        CondExpr {
            name: Box::new(name),
            clauses: awaitables,
        }
    }
    pub fn place_holder() -> Self {
        CondExpr {
            name: Box::new(None),
            clauses: vec![],
        }
    }
    pub fn add_name(&mut self, name: Option<Ident>) {
        self.name = Box::new(name);
    }
    pub fn add_awaitable(&mut self, awaitable: Awaitable) {
        self.clauses.push(awaitable);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Awaitable{
    FnCall(FnCall),
    VariableCallExpr(VariableCallExpr),
    Comp(Comp),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ActExpr {
    name: Box<Option<Ident>>,
    operatable: Vec<Stmt>,
}

impl ActExpr {
    pub fn new(name: Option<Ident>, operatable: Vec<Stmt>) -> Self {
        ActExpr {
            name: Box::new(name),
            operatable: operatable,
        }
    }
    pub fn place_holder() -> Self {
        ActExpr {
            name: Box::new(None),
            operatable: vec![],
        }
    }
    pub fn add_name(&mut self, name: Option<Ident>) {
        self.name = Box::new(name);
    }
    pub fn add_operatable(&mut self, stmt: Stmt) {
        self.operatable.push(stmt);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableCallExpr {
    pub variables: Vec<Callable>,
}

impl VariableCallExpr {
    pub fn new(variables: Vec<Callable>) -> Self {
        VariableCallExpr {
            variables: variables,
        }
    }
    pub fn place_holder() -> Self {
        VariableCallExpr { variables: vec![] }
    }
    pub fn add_variable(&mut self, callable: Callable) {
        self.variables.push(callable);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callable {
    FnCall(FnCall),
    Ident(Ident),
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
    HexLit(HexLit),
    BinaryLit(BinaryLit),
    UnicordLit(UnicordLit),
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
    pub value: Box<Ident>, // At this point, number type cannot be determined
}

impl NumberLit {
    pub fn new(val: &str) -> Self {
        NumberLit {
            value: Box::new(Ident::new(val, None)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryLit {
    value: Box<String>,
}

impl BinaryLit {
    pub fn new(val: &str) -> Self {
        BinaryLit {
            value: Box::new(String::from(val)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HexLit {
    value: Box<String>,
}

impl HexLit {
    pub fn new(val: &str) -> Self {
        HexLit {
            value: Box::new(String::from(val)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnicordLit {
    value: Box<String>,
}

impl UnicordLit {
    pub fn new(val: &str) -> Self {
        UnicordLit {
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
