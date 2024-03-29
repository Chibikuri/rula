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

    pub fn eoi() -> RuLa {
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
    Repeaters,
    // Higher level expressions
    Import(Import),
    RuleSetExpr(RuleSetExpr),
    RuleExpr(RuleExpr),
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
    If(If),
    For(For),
    Match(Match),
    Promote(Promote),
    Send(Send),
    Set(Set),
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
    pub ident: Vec<Ident>, // ExprKind::Ident
    pub expr: Box<Expr>,
}

impl Let {
    // Constructor with kind check
    pub fn new(identifiers: Vec<Ident>, expr: Expr) -> Self {
        Let {
            ident: identifiers,
            expr: Box::new(expr),
        }
    }

    pub fn place_holder() -> Self {
        Let {
            ident: vec![],
            expr: Box::new(Expr::place_holder()),
        }
    }

    pub fn add_ident(&mut self, identifier: Ident) {
        self.ident.push(identifier);
    }

    pub fn add_expr(&mut self, expr: Expr) {
        self.expr = Box::new(expr);
    }
}

/**
 * Import functions with path name
 *
 *  `path`: Vec<PathBuf> path to some function and Rule
 *  `rule_import`: bool annotation for rule import
 *  `imported_rules`: RuLa AST imported from other files
*/
#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub path: Vec<PathBuf>,
    pub rule_import: bool,
    pub imported_rules: Vec<RuleExpr>,
    pub rule_names: Vec<String>,
    // import statement defined in the other files.
    // TODO rule import cannot be nested to avoid circular reference.
    pub local_importing: Vec<Import>,
}

impl Import {
    pub fn new(
        path: Vec<PathBuf>,
        rule_import: bool,
        rules: Vec<RuleExpr>,
        rule_names: Vec<String>,
        local_importing: Vec<Import>,
    ) -> Self {
        Import {
            path: path,
            rule_import: rule_import,
            imported_rules: rules,
            rule_names: rule_names,
            local_importing: local_importing,
        }
    }

    pub fn place_holder() -> Self {
        Import {
            path: vec![],
            rule_import: false,
            imported_rules: vec![],
            rule_names: vec![],
            local_importing: vec![],
        }
    }

    pub fn add_path(&mut self, path: PathBuf) {
        self.path.push(path);
    }

    pub fn set_path(&mut self, paths: Vec<PathBuf>) {
        self.path = paths
    }

    pub fn add_rule(&mut self, rule: RuleExpr) {
        self.imported_rules.push(rule);
    }

    pub fn set_rules(&mut self, rules: Vec<RuleExpr>) {
        self.imported_rules = rules;
    }

    pub fn add_rule_name(&mut self, rule_name: String) {
        self.rule_names.push(rule_name)
    }

    pub fn set_rule_names(&mut self, rule_names: Vec<String>) {
        self.rule_names = rule_names;
    }

    pub fn rule_import(&mut self) {
        self.rule_import = true;
    }

    pub fn set_local_import(&mut self, local_import: Vec<Import>) {
        self.local_importing = local_import;
    }

    pub fn add_local_import(&mut self, import: Import) {
        self.local_importing.push(import);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RuleSetExpr {
    pub name: Box<Ident>,
    pub rules: Vec<Stmt>,
}

impl RuleSetExpr {
    pub fn new(name: Ident, rules: Vec<Stmt>) -> Self {
        RuleSetExpr {
            name: Box::new(name),
            rules: rules,
        }
    }
    pub fn place_holder() -> Self {
        RuleSetExpr {
            name: Box::new(Ident::place_holder()),
            rules: vec![],
        }
    }
    pub fn add_name(&mut self, name: Ident) {
        self.name = Box::new(name);
    }
    pub fn add_stmt(&mut self, stmt: Stmt) {
        self.rules.push(stmt);
    }
}

// rule_stmt = {^"rule" ~ angle_expr ~ arguments ~ brace_stmt}
#[derive(Debug, Clone, PartialEq)]
pub struct RuleExpr {
    pub name: Box<Ident>,
    pub repeater_ident: Box<Ident>,
    pub args: Vec<Ident>,
    pub ret_type_annotation: Box<Option<ReturnTypeAnnotation>>,
    pub rule_content: Box<RuleContentExpr>,
}

impl RuleExpr {
    pub fn new(
        name: Ident,
        repeater_ident: Ident,
        arg_vec: Vec<Ident>,
        ret_type_annotation: Option<ReturnTypeAnnotation>,
        rule_content: RuleContentExpr,
    ) -> Self {
        RuleExpr {
            name: Box::new(name),
            repeater_ident: Box::new(repeater_ident),
            args: arg_vec,
            ret_type_annotation: Box::new(ret_type_annotation),
            rule_content: Box::new(rule_content),
        }
    }
    pub fn place_holder() -> Self {
        RuleExpr {
            name: Box::new(Ident::place_holder()),
            repeater_ident: Box::new(Ident::place_holder()),
            args: vec![],
            ret_type_annotation: Box::new(None),
            rule_content: Box::new(RuleContentExpr::place_holder()),
        }
    }
    pub fn add_name(&mut self, name: Ident) {
        self.name = Box::new(name);
    }

    pub fn add_repeater_ident(&mut self, name: Ident) {
        self.repeater_ident = Box::new(name);
    }

    pub fn add_arg(&mut self, arg: Ident) {
        self.args.push(arg);
    }

    pub fn add_return_type_annotation(
        &mut self,
        ret_type_annotation: Option<ReturnTypeAnnotation>,
    ) {
        self.ret_type_annotation = Box::new(ret_type_annotation);
    }
    pub fn add_rule_content(&mut self, rule_content: RuleContentExpr) {
        self.rule_content = Box::new(rule_content)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnTypeAnnotation {
    pub typedefs: Vec<(TypeDef, bool)>,
}

impl ReturnTypeAnnotation {
    pub fn new(typedef: Vec<(TypeDef, bool)>) -> Self {
        ReturnTypeAnnotation { typedefs: typedef }
    }
    pub fn place_holder() -> Self {
        ReturnTypeAnnotation { typedefs: vec![] }
    }

    pub fn add_type_def(&mut self, type_def: TypeDef) {
        self.typedefs.push((type_def, false));
    }
    pub fn update_maybe(&mut self) {
        let last_val = self.typedefs.pop().unwrap();
        self.typedefs.push((last_val.0, true));
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RuleContentExpr {
    pub pre_processing: Vec<Let>,
    pub condition_expr: Box<CondExpr>,
    pub action_expr: Box<ActExpr>,
    pub post_processing: Vec<Stmt>,
}

impl RuleContentExpr {
    pub fn new(
        pre_processing: Vec<Let>,
        condition_expr: CondExpr,
        action_expr: ActExpr,
        post_processing: Vec<Stmt>,
    ) -> Self {
        RuleContentExpr {
            pre_processing: pre_processing,
            condition_expr: Box::new(condition_expr),
            action_expr: Box::new(action_expr),
            post_processing: post_processing,
        }
    }
    pub fn place_holder() -> Self {
        RuleContentExpr {
            pre_processing: vec![],
            condition_expr: Box::new(CondExpr::place_holder()),
            action_expr: Box::new(ActExpr::place_holder()),
            post_processing: vec![],
        }
    }
    pub fn add_pre_let_stmt(&mut self, let_stmt: Let) {
        self.pre_processing.push(let_stmt);
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
pub struct CondExpr {
    pub name: Box<Option<Ident>>,
    pub clauses: Vec<CondClauses>,
}

impl CondExpr {
    pub fn new(name: Option<Ident>, cond_clauses: Vec<CondClauses>) -> Self {
        CondExpr {
            name: Box::new(name),
            clauses: cond_clauses,
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
    pub fn add_condition_clause(&mut self, awaitable: CondClauses) {
        self.clauses.push(awaitable);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CondClauses {
    ResAssign(ResAssign),
    FnCall(FnCall),
    VariableCallExpr(VariableCallExpr),
    Comp(Comp),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResAssign {
    pub res_name: Box<Ident>,
    pub fn_call: Box<FnCall>,
}

impl ResAssign {
    pub fn new(res_name: Ident, fn_call: FnCall) -> Self {
        ResAssign {
            res_name: Box::new(res_name),
            fn_call: Box::new(fn_call),
        }
    }
    pub fn place_holder() -> Self {
        ResAssign {
            res_name: Box::new(Ident::place_holder()),
            fn_call: Box::new(FnCall::place_holder()),
        }
    }

    pub fn add_res_name(&mut self, name: Ident) {
        self.res_name = Box::new(name);
    }
    pub fn add_fn_call(&mut self, fn_call: FnCall) {
        self.fn_call = Box::new(fn_call);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ActExpr {
    pub name: Box<Option<Ident>>,
    pub operatable: Vec<Stmt>,
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
    pub block: Box<IfBlock>, // StmtKind::Expr
    pub stmts: Vec<Stmt>,
    pub elif: Vec<If>,
    pub els: Vec<Stmt>,
}

impl If {
    pub fn new(block: IfBlock, stmt: Vec<Stmt>, elif: Vec<If>, els: Vec<Stmt>) -> If {
        If {
            // enum type validation
            block: Box::new(block),
            stmts: stmt,
            elif: elif,
            els: els,
        }
    }

    pub fn place_holder() -> If {
        If {
            block: Box::new(IfBlock::PlaceHolder),
            stmts: vec![],
            elif: vec![],
            els: vec![],
        }
    }

    pub fn add_block(&mut self, block: IfBlock) {
        self.block = Box::new(block);
    }

    pub fn add_stmt(&mut self, stmt: Stmt) {
        self.stmts.push(stmt)
    }

    pub fn add_elif(&mut self, elif: If) {
        self.elif.push(elif);
    }

    pub fn add_else(&mut self, els: Stmt) {
        self.els.push(els);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfBlock {
    Get(Get),
    Comp(Comp),
    Lit(Lit),
    PlaceHolder,
}

// for_stmt = { ^"for" ~ "(" ~ pattern ~")"~ "in" ~ expr ~ brace_stmt }
#[derive(Debug, Clone, PartialEq)]
pub struct For {
    pub variables: Vec<Ident>,
    pub generator: Box<ForGenerator>,
    pub stmts: Vec<Stmt>,
}

impl For {
    pub fn new(idents: Vec<Ident>, expression: ForGenerator, statements: Vec<Stmt>) -> Self {
        For {
            variables: idents,
            generator: Box::new(expression),
            stmts: statements,
        }
    }
    pub fn place_holder() -> Self {
        For {
            variables: vec![],
            generator: Box::new(ForGenerator::PlaceHolder),
            stmts: vec![],
        }
    }
    pub fn add_ident(&mut self, ident: Ident) {
        self.variables.push(ident);
    }
    pub fn add_generator(&mut self, generator: ForGenerator) {
        self.generator = Box::new(generator);
    }
    pub fn add_stmt(&mut self, stmt: Stmt) {
        self.stmts.push(stmt);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForGenerator {
    Series(Series),
    Expr(Expr),
    PlaceHolder,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Series {
    pub start: Box<IntegerLit>,
    pub end: Box<Expr>,
}

impl Series {
    pub fn new(start: IntegerLit, end: Expr) -> Self {
        Series {
            start: Box::new(start),
            end: Box::new(end),
        }
    }
    pub fn place_holder() -> Self {
        Series {
            start: Box::new(IntegerLit::place_holder()),
            end: Box::new(Expr::place_holder()),
        }
    }

    pub fn add_start(&mut self, start: IntegerLit) {
        self.start = Box::new(start);
    }
    pub fn add_end(&mut self, end: Expr) {
        self.end = Box::new(end);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Match {
    pub expr: Box<Expr>,
    pub match_arms: Vec<MatchArm>,
    pub otherwise: Box<Option<MatchAction>>,
}

impl Match {
    pub fn new(expr: Expr, match_arms: Vec<MatchArm>, otherwise: Option<MatchAction>) -> Self {
        Match {
            expr: Box::new(expr),
            match_arms: match_arms,
            otherwise: Box::new(otherwise),
        }
    }

    pub fn place_holder() -> Self {
        Match {
            expr: Box::new(Expr::place_holder()),
            match_arms: vec![],
            otherwise: Box::new(None),
        }
    }
    pub fn add_expr(&mut self, expr: Expr) {
        self.expr = Box::new(expr);
    }
    pub fn add_match_arm(&mut self, match_arm: MatchArm) {
        self.match_arms.push(match_arm);
    }
    pub fn add_finally(&mut self, finally: Option<MatchAction>) {
        self.otherwise = Box::new(finally);
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
    pub satisfiable: Box<Satisfiable>,
}

impl MatchCondition {
    pub fn new(satisfiable: Satisfiable) -> Self {
        MatchCondition {
            satisfiable: Box::new(satisfiable),
        }
    }
    pub fn place_holder() -> Self {
        MatchCondition {
            satisfiable: Box::new(Satisfiable::PlaceHolder),
        }
    }
    pub fn update_satisfiable(&mut self, satisfiable: Satisfiable) {
        self.satisfiable = Box::new(satisfiable);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Satisfiable {
    // Comp(Comp),
    // Ident(Ident),
    Lit(Lit),
    PlaceHolder,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchAction {
    pub actionable: Vec<Stmt>,
}

impl MatchAction {
    pub fn new(actionable: Vec<Stmt>) -> Self {
        MatchAction {
            actionable: actionable,
        }
    }
    pub fn place_holder() -> Self {
        MatchAction { actionable: vec![] }
    }
    pub fn add_actionable(&mut self, actionable: Stmt) {
        self.actionable.push(actionable);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Promote {
    pub target: Vec<Promotables>,
}

impl Promote {
    pub fn new(promote_target: Vec<Promotables>) -> Self {
        Promote {
            target: promote_target,
        }
    }
    pub fn place_holder() -> Self {
        Promote { target: vec![] }
    }
    pub fn add_target(&mut self, target: Promotables) {
        self.target.push(target);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Promotables {
    Comp(Comp),
    Term(Term),
    RuLaVec(RuLaVec),
    RuLaTuple(RuLaTuple),
    VariableCall(VariableCallExpr),
    Lit(Lit),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Send {
    pub fn_call: Box<FnCall>,
    // This could be limited more
    pub expr: Box<Expr>,
}

impl Send {
    pub fn new(fn_call: FnCall, expr: Expr) -> Self {
        Send {
            fn_call: Box::new(fn_call),
            expr: Box::new(expr),
        }
    }
    pub fn place_holder() -> Self {
        Send {
            fn_call: Box::new(FnCall::place_holder()),
            expr: Box::new(Expr::place_holder()),
        }
    }
    pub fn add_fn_call(&mut self, fn_call: FnCall) {
        self.fn_call = Box::new(fn_call);
    }
    pub fn add_expr(&mut self, expr: Expr) {
        self.expr = Box::new(expr);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Set {
    pub value: Box<Ident>,
    pub alias: Box<Option<Ident>>,
}

impl Set {
    pub fn new(value: Ident, alias: Option<Ident>) -> Self {
        Set {
            value: Box::new(value),
            alias: Box::new(alias),
        }
    }
    pub fn place_holder() -> Self {
        Set {
            value: Box::new(Ident::place_holder()),
            alias: Box::new(None),
        }
    }
    pub fn set_value(&mut self, value: Ident) {
        self.value = Box::new(value);
    }
    pub fn set_alias(&mut self, alias: Option<Ident>) {
        self.alias = Box::new(alias);
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
    Get(Get),
    FnCall(FnCall),
    RuleCall(RuleCall),
    RuLaVec(RuLaVec),
    RuLaTuple(RuLaTuple),
    Comp(Comp),
    Term(Term), // There could be better way. Leave this for now.
    VariableCallExpr(VariableCallExpr),
    Lit(Lit),
    PlaceHolder, // for initializing reason, but maybe better way?
}

#[derive(Debug, Clone, PartialEq)]
pub struct Get {
    pub value: Box<Ident>,
}

impl Get {
    pub fn new(value: Ident) -> Self {
        Get {
            value: Box::new(value),
        }
    }
    pub fn place_holder() -> Self {
        Get {
            value: Box::new(Ident::place_holder()),
        }
    }
    pub fn set_value(&mut self, value: Ident) {
        self.value = Box::new(value);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnCall {
    pub func_name: Box<Ident>,
    // If this is just a repeater call, true
    pub repeater_call: bool,
    pub arguments: Vec<FnCallArgs>,
}

impl FnCall {
    pub fn new(name: Ident, repeater_call: bool, arguments: Vec<FnCallArgs>) -> Self {
        FnCall {
            func_name: Box::new(name),
            repeater_call: repeater_call,
            arguments: arguments,
        }
    }

    pub fn place_holder() -> Self {
        FnCall {
            func_name: Box::new(Ident::place_holder()),
            repeater_call: false,
            arguments: vec![],
        }
    }

    pub fn add_name(&mut self, name: Ident) {
        self.func_name = Box::new(name);
    }
    pub fn update_repeater_call(&mut self) {
        self.repeater_call = true;
    }
    pub fn add_argument(&mut self, arg: FnCallArgs) {
        self.arguments.push(arg);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FnCallArgs {
    FnCall(FnCall),
    VariableCall(VariableCallExpr),
    Lit(Lit),
    Term(Term),
}

#[derive(Debug, Clone, PartialEq)]
pub struct RuleCall {
    pub rule_name: Box<Ident>,
    pub repeater_arg: Box<RepeaterCallArg>,
    pub argument: Vec<FnCallArgs>,
}

impl RuleCall {
    pub fn new(
        rule_name: Ident,
        repeater_arg: RepeaterCallArg,
        arguments: Vec<FnCallArgs>,
    ) -> Self {
        RuleCall {
            rule_name: Box::new(rule_name),
            repeater_arg: Box::new(repeater_arg),
            argument: arguments,
        }
    }

    pub fn place_holder() -> Self {
        RuleCall {
            rule_name: Box::new(Ident::place_holder()),
            repeater_arg: Box::new(RepeaterCallArg::PlaceHolder),
            argument: vec![],
        }
    }

    pub fn update_rule_name(&mut self, new_name: Ident) {
        self.rule_name = Box::new(new_name);
    }

    pub fn add_repeater_arg(&mut self, repeater_arg: RepeaterCallArg) {
        self.repeater_arg = Box::new(repeater_arg);
    }

    pub fn add_arg(&mut self, arg: FnCallArgs) {
        self.argument.push(arg);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RepeaterCallArg {
    Term(Term),
    Ident(Ident),
    IntegerLit(IntegerLit),
    PlaceHolder,
}
#[derive(Debug, Clone, PartialEq)]
pub struct Comp {
    pub lhs: Box<Comparable>, // comparable expression (should be traited later)
    pub comp_op: Box<CompOpKind>,
    pub rhs: Box<Comparable>,
}

impl Comp {
    pub fn new(lhs: Comparable, comp_op: CompOpKind, rhs: Comparable) -> Self {
        Comp {
            lhs: Box::new(lhs),
            comp_op: Box::new(comp_op),
            rhs: Box::new(rhs),
        }
    }
    pub fn place_holder() -> Self {
        Comp {
            lhs: Box::new(Comparable::PlaceHolder),
            comp_op: Box::new(CompOpKind::PlaceHolder),
            rhs: Box::new(Comparable::PlaceHolder),
        }
    }
    pub fn add_lhs(&mut self, lhs: Comparable) {
        self.lhs = Box::new(lhs)
    }
    pub fn add_rhs(&mut self, rhs: Comparable) {
        self.rhs = Box::new(rhs)
    }
    pub fn add_comp_op(&mut self, op: CompOpKind) {
        self.comp_op = Box::new(op)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Comparable {
    Get(Get),
    Term(Term),
    VariableCall(VariableCallExpr),
    FnCall(FnCall),
    Lit(Lit),
    PlaceHolder,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Term {
    pub lhs: Box<Terms>,
    pub op: Box<TermOps>,
    pub rhs: Box<Terms>,
}

impl Term {
    pub fn new(lhs: Terms, op: TermOps, rhs: Terms) -> Self {
        Term {
            lhs: Box::new(lhs),
            op: Box::new(op),
            rhs: Box::new(rhs),
        }
    }

    pub fn place_holder() -> Self {
        Term {
            lhs: Box::new(Terms::PlaceHolder),
            op: Box::new(TermOps::PlaceHolder),
            rhs: Box::new(Terms::PlaceHolder),
        }
    }
    pub fn add_lhs(&mut self, lhs: Terms) {
        self.lhs = Box::new(lhs);
    }
    pub fn add_op(&mut self, op: TermOps) {
        self.op = Box::new(op);
    }
    pub fn add_rhs(&mut self, rhs: Terms) {
        self.rhs = Box::new(rhs);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Terms {
    Term(Term),
    VariableCallExpr(VariableCallExpr),
    FnCall(FnCall),
    Lit(Lit),
    PlaceHolder,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TermOps {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Caret,
    PlaceHolder,
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
    RepeaterIdent(Ident),
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
pub struct RuLaVec {
    // pub type_hint: Box<TypeDef>,
    pub items: Vec<Lit>,
}

impl RuLaVec {
    pub fn new(item_vec: Vec<Lit>) -> Self {
        RuLaVec {
            // type_hint: Box::new(type_val),
            items: item_vec,
        }
    }
    pub fn place_holder() -> Self {
        RuLaVec { items: vec![] }
    }

    pub fn add_item(&mut self, item: Lit) {
        self.items.push(item)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RuLaTuple {
    pub items: Vec<Expr>,
}

impl RuLaTuple {
    pub fn new(item_tuple: Vec<Expr>) -> Self {
        RuLaTuple { items: item_tuple }
    }

    pub fn place_holder() -> Self {
        RuLaTuple { items: vec![] }
    }

    pub fn add_item(&mut self, item: Expr) {
        self.items.push(item);
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
    pub string: Box<String>,
}

impl StringLit {
    pub fn new(strs: &str) -> StringLit {
        StringLit {
            string: Box::new(String::from(strs)),
        }
    }
    pub fn as_string(&self) -> String {
        *self.string.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NumberLit {
    pub kind: NumberLitKind,
}

impl NumberLit {
    pub fn new(kind: NumberLitKind) -> Self {
        NumberLit { kind: kind }
    }

    pub fn place_holder() -> Self {
        NumberLit {
            kind: NumberLitKind::PlaceHolder,
        }
    }

    pub fn update_value(&mut self, kind: NumberLitKind) {
        self.kind = kind;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumberLitKind {
    IntegerLit(IntegerLit),
    FloatLit(FloatLit),
    NumIdentLit(NumIdentLit),
    PlaceHolder,
}

impl NumberLit {
    pub fn negative(&mut self) {
        match &mut self.kind {
            NumberLitKind::IntegerLit(int_lit) => {
                int_lit.negative();
            }
            NumberLitKind::FloatLit(float_lit) => {
                float_lit.negative();
            }
            NumberLitKind::NumIdentLit(num_ident_lit) => num_ident_lit.negative(),
            NumberLitKind::PlaceHolder => {
                panic!("Value is not properly set");
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLit {
    pub value: Box<Ident>,
    pub negative: bool,
}

impl IntegerLit {
    pub fn new(val: &str, negative: bool) -> Self {
        IntegerLit {
            value: Box::new(Ident::new(val, Some(TypeDef::Integer))),
            negative: negative,
        }
    }

    pub fn place_holder() -> Self {
        IntegerLit {
            value: Box::new(Ident::place_holder()),
            negative: false,
        }
    }

    pub fn negative(&mut self) {
        self.negative = true;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FloatLit {
    pub value: Box<Ident>,
    pub negative: bool,
}

impl FloatLit {
    pub fn new(val: &str, negative: bool) -> Self {
        FloatLit {
            value: Box::new(Ident::new(val, Some(TypeDef::Float))),
            negative: negative,
        }
    }

    pub fn negative(&mut self) {
        self.negative = true;
    }
}

// Identifier for a number.
// At the parser, this cannot be classified to int or float
#[derive(Debug, Clone, PartialEq)]
pub struct NumIdentLit {
    pub value: Box<Ident>,
    pub negative: bool,
}

impl NumIdentLit {
    pub fn new(val: Ident, negative: bool) -> Self {
        NumIdentLit {
            value: Box::new(val),
            negative: negative,
        }
    }

    pub fn place_holder() -> Self {
        NumIdentLit {
            value: Box::new(Ident::place_holder()),
            negative: false,
        }
    }

    pub fn negative(&mut self) {
        self.negative = true;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryLit {
    pub value: Box<String>,
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
    pub value: Box<String>,
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
    pub value: Box<String>,
}

impl UnicordLit {
    pub fn new(val: &str) -> Self {
        UnicordLit {
            value: Box::new(String::from(val)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeDef {
    Repeater,
    Message,
    Result,
    Qubit,
    Integer,
    UnsignedInteger,
    Float,
    Boolean,
    Str,
    Vector(Box<TypeDef>),
    PlaceHolder,
}
