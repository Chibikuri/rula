use super::conf_parser::parse_config;
use super::error::RuleSetGenError;
use super::ruleset::{Rule, RuleSet, Stage};
use super::tracker::{
    Arguments, LocalVariableTracker, RetTypeAnnotation, RuleGen, Tracker, VarKind, Variable,
};
use super::types::*;
use super::IResult;
use rula_parser::parser::ast::*;

use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;

type RuleVec = Rc<RefCell<Vec<Rule>>>;
type LocalValTrack = Rc<RefCell<LocalVariableTracker>>;

pub type Scope = String;
pub type Closure<T, U, V> = Box<dyn Fn(T, U) -> V>;
pub type ValueTracker = RefCell<Tracker>;

/// Generate corresponding rust code from ast
/// Every nested generators returns a piece of TokenStream
/// Arguments:
///     ast_tree
pub fn generate(ast_tree: &AstNode, config_path: PathBuf) -> IResult<Vec<RuleSet>> {
    // RuLa should know how many repeaters inconfig at this moment

    // Initialize tracker to track global state over the functions
    let tracker = RefCell::new(Tracker::new());

    parse_config(config_path, &tracker).unwrap();
    let num_node = tracker.borrow().repeaters.len();
    // Add empty RuleSets
    for i in 0..num_node {
        tracker.borrow_mut().add_ruleset(i, RuleSet::new("empty"));
    }
    // Generated rula program
    match ast_tree {
        // All RuLa AST starts RuLa Node
        AstNode::RuLa(rula) => generate_rula(rula, &tracker).unwrap(),
        AstNode::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    };

    // Finally, there are evaluatable RuleSet is inside the tracker
    tracker.borrow().generate_ruleset(&tracker);
    // return generated rulesets
    let rulesets = tracker.borrow().return_rulesets();
    Ok(rulesets)
}

// Generate entire rula program
pub(super) fn generate_rula(rula: &RuLa, tracker: &ValueTracker) -> IResult<()> {
    match &*rula.rula {
        RuLaKind::Program(program) => {
            generate_program(program, tracker).unwrap();
        }
        RuLaKind::Ignore => {}
        RuLaKind::Eoi => {}
        RuLaKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    }
    Ok(())
}

// Generate progra
// program: Program AST that contains a vector of Stmt
pub(super) fn generate_program(program: &Program, tracker: &ValueTracker) -> IResult<()> {
    // A vector to store a set of generated stmts
    for program_kind in &program.programs {
        // Right now, a program can takes a stmt
        match program_kind {
            ProgramKind::Repeaters => {
                // Prepare annotation
            }
            ProgramKind::Import(import_expr) => {
                generate_import(import_expr, tracker).unwrap();
            }
            ProgramKind::RuleSetExpr(ruleset_expr) => {
                generate_ruleset_expr(ruleset_expr, tracker).unwrap();
            }
            ProgramKind::RuleExpr(rule_expr) => {
                generate_rule_expr(rule_expr, tracker).unwrap();
            }
        }
    }
    Ok(())
}

pub(super) fn generate_import(import_expr: &Import, tracker: &ValueTracker) -> IResult<()> {
    // How to get the other rules?
    Ok(())
}

pub(super) fn generate_ruleset_expr(
    ruleset_expr: &RuleSetExpr,
    tracker: &ValueTracker,
) -> IResult<()> {
    // 0. Get the ruleset name and update the ruleset names
    let ruleset_name = &*ruleset_expr.name.name;
    if tracker.borrow().check_rule_name_exist(ruleset_name) {
        return Err(RuleSetGenError::SameNameExistInRuleError);
    }
    // Register ruleset name no to be used by other rule as well
    tracker.borrow_mut().register_rule_name(&ruleset_name);
    tracker.borrow_mut().update_ruleset_name(ruleset_name);

    // 1. Introduce ruleset scope
    let scope = ruleset_name;

    for stmt in &ruleset_expr.rules {
        generate_stmt(stmt, tracker, scope).unwrap();
    }

    // RuleSet Generator closure
    let ruleset_generator = Box::new(|tracker: &ValueTracker, scope: &Scope| {
        let mut rulesets: Vec<RuleSet> = vec![];
        // Evaluate all statements here
        rulesets
    });

    tracker
        .borrow_mut()
        .register_ruleset_generator(ruleset_generator);
    // fin. Release all scoped values here
    Ok(())
}

pub(super) fn generate_rule_expr(rule_expr: &RuleExpr, tracker: &ValueTracker) -> IResult<()> {
    // 0. Get the rule name and register it to the tracker
    let rule_name = &*rule_expr.name.name;
    if tracker.borrow().check_rule_name_exist(rule_name) {
        return Err(RuleSetGenError::RuleNameDuplicationError);
    }
    tracker.borrow_mut().register_rule_name(&rule_name);

    // Introduce scope here
    let scope = rule_name;

    // 1. Register repeater identifiers to be used in this RuleExpr
    // e.g. <#rep> This can be refered as repeater value
    // Now the number of repeater is limited to 1
    let repeater_identifier = &*rule_expr.repeater_ident.name;
    tracker
        .borrow_mut()
        .add_internal_repeater_name(rule_name, repeater_identifier);
    // 2. Register given arguments to be used in this RuleExpr
    // e.g.
    let argument_identifiers = {
        let mut argument_names = vec![];
        for arg in &*rule_expr.args {
            argument_names.push(arg.name.to_string());
        }
        argument_names
    };
    tracker
        .borrow_mut()
        .add_internal_argument_names(rule_name, argument_identifiers);

    // 3. Check if there is a type annotation for return value or not
    let mut return_types = RetTypeAnnotation::new(scope);
    match &*rule_expr.ret_type_annotation {
        Some(type_annoation) => {
            for (type_val, maybe) in &type_annoation.typedefs {
                return_types.add_return_type(generate_type_hint(&type_val).unwrap(), *maybe);
            }
        }
        None => {}
    }
    tracker
        .borrow_mut()
        .add_return_type_annotation(rule_name, return_types);

    // 4. Move into the RuleContent
    // This has to be evaluated when the rule is called.
    // As long as the arguments are given, this rule can be executed
    let rule_generator = generate_rule_content(&rule_expr.rule_content, tracker, scope).unwrap();

    let rule_generator = move |repeater: Repeater,
                               arguments: Arguments,
                               tracker: &ValueTracker,
                               scope: &Scope| {
        let local_tracker = Rc::new(RefCell::new(LocalVariableTracker::new()));
        // Currently only one repeater can be inside this
        local_tracker.borrow_mut().register(
            &tracker.borrow().get_internal_repeater_name(&scope),
            &RuLaValue::Repeater(repeater),
        );

        let num_args = arguments.vals.len();
        if num_args != tracker.borrow().get_internal_argument_names(&scope).len() {
            panic!("Something Failed internally. The number of arguments doesn't match it is supposed to be");
        }
        for (index, arg_name) in tracker
            .borrow()
            .get_internal_argument_names(&scope)
            .iter()
            .enumerate()
        {
            local_tracker
                .borrow_mut()
                .register(arg_name, &arguments.vals[index])
        }
        // Initial RuleSettings
        // The number of rules would be larger and larger
        let rules = Rc::new(RefCell::new(vec![Rule::new(&scope)]));
        let generated_rules = rule_generator(Rc::clone(&rules), Rc::clone(&local_tracker));
        let mut stage = Stage::new();
        for rl in &*generated_rules.borrow() {
            stage.add_rule(rl.clone());
        }
        stage
    };
    tracker
        .borrow_mut()
        .add_ruleset_generator(rule_name, Box::new(rule_generator));

    // 5. Release all the scope within this Rule
    // tracker.borrow_mut().clean_scope(scope);
    Ok(())
}

pub(super) fn generate_rule_content(
    rule_content_expr: &RuleContentExpr,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<Closure<RuleVec, LocalValTrack, RuleVec>> {
    // 0. Pre processing (local variable assignment)
    // Variables should be registered in this process
    // for let_stmt in &rule_content_expr.pre_processing {
    //     // register local variables with scope
    //     generate_let(let_stmt, tracker, scope).unwrap();
    // }

    // 1. condition clause
    let condition_generator =
        generate_cond(&rule_content_expr.condition_expr, tracker, scope).unwrap();
    // 2. action clause

    let act_generator = generate_act(&*rule_content_expr.action_expr, tracker, scope).unwrap();

    // 3. Post processing
    let rule_content_generator = move |rules: RuleVec, local_val_tracker: LocalValTrack| {
        // Prepare local value tracker to track the local varibale
        // 0. value assignment with current tracker
        // Right now, all arguments are cloned but this should be cleaned with Rc and RefCell in the future
        condition_generator(Rc::clone(&rules), Rc::clone(&local_val_tracker));
        act_generator(Rc::clone(&rules), Rc::clone(&local_val_tracker));
        rules
    };

    Ok(Box::new(rule_content_generator))
}

pub(super) fn generate_cond(
    cond_expr: &CondExpr,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<Closure<RuleVec, LocalValTrack, RuleVec>> {
    Ok(Box::new(|rules, local_tracker| rules))
}

pub(super) fn generate_act(
    act_expr: &ActExpr,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<Closure<RuleVec, LocalValTrack, RuleVec>> {
    Ok(Box::new(|rules, var_tracker| rules))
}

// Generate stmt expression
// Arguments:
//  stmt: Stmt AST
//  rule_name: Option<String> a name of corresponding Rule
//  ident_tracker: List of identifiers that need to be tracked
pub(super) fn generate_stmt(stmt: &Stmt, tracker: &ValueTracker, scope: &Scope) -> IResult<()> {
    match &*stmt.kind {
        StmtKind::Let(let_stmt) => {
            // generate_let(let_stmt, tracker, scope).unwrap();
        }
        StmtKind::Expr(expr) => {
            generate_expr(expr, tracker, scope).unwrap();
        }
        StmtKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    }
    Ok(())
}

/// Create a new variable that can be used inside the rule
/// Let statement doesn't affect rules. Just update local trackers
pub(super) fn generate_let(
    let_stmt: &Let,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<Closure<(), LocalVariableTracker, ()>> {
    // Register a variable to the tracker within the ruleset or rule
    let value = generate_expr(&*let_stmt.expr, tracker, scope).unwrap();
    let variable_name = &*let_stmt.ident.name;
    match &*let_stmt.ident.type_hint {
        Some(hint) => {
            // tracker.borrow_mut().register_local_variable(
            //     variable_name,
            //     Variable::new(
            //         variable_name,
            //         scope,
            //         VarKind::LetAssignment,
            //         generate_type_hint(hint).unwrap(),
            //     ),
            // );
        }
        None => return Err(RuleSetGenError::NoTypeAnnotationError),
    }
    Ok(Box::new(|_, tracker| { /* update tracker here*/ }))
}

pub(super) fn generate_expr(expr: &Expr, tracker: &ValueTracker, scope: &Scope) -> IResult<()> {
    match &*expr.kind {
        ExprKind::If(if_expr) => {
            generate_if(if_expr, tracker).unwrap();
        }
        ExprKind::For(for_expr) => generate_for(for_expr, tracker).unwrap(),
        ExprKind::Match(match_expr) => {
            generate_match(match_expr, tracker).unwrap();
        }
        ExprKind::Return(return_expr) => {
            generate_return(return_expr, tracker).unwrap();
        }
        ExprKind::Send(send_expr) => {
            generate_send(send_expr, tracker).unwrap();
        }
        ExprKind::FnCall(fn_call_expr) => generate_fn_call(fn_call_expr, tracker).unwrap(),
        ExprKind::RuleCall(rule_call_expr) => generate_rule_call(rule_call_expr, tracker).unwrap(),
        ExprKind::Comp(comp_expr) => generate_comp(comp_expr, tracker, scope).unwrap(),
        ExprKind::Term(term_expr) => generate_term(term_expr, tracker).unwrap(),
        ExprKind::VariableCallExpr(variable_call_expr) => {
            generate_variable_call(variable_call_expr, tracker).unwrap()
        }
        ExprKind::Lit(lit_expr) => {
            generate_lit(&lit_expr, tracker).unwrap();
        }
        ExprKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    }
    Ok(())
}

pub(super) fn generate_if(if_expr: &If, tracker: &ValueTracker) -> IResult<()> {
    // if (block) {expression}
    // // (block)
    // let first_condition = generate_expr(
    //     &mut *if_expr.block,
    //     tracker
    // )
    // .unwrap();
    // let generated_stmt = {
    //     let mut gen_stmt = vec![];
    //     for stmt in &mut if_expr.stmts {
    //         gen_stmt.push(generate_stmt(stmt, tracker).unwrap());
    //     }
    //     gen_stmt
    // };

    // let generated_els = match &mut *if_expr.els {
    //     Some(els) => {
    //         let els_stmt = generate_stmt(els, tracker).unwrap();
    //     }
    //     None => quote!(),
    // };

    // let generated_elifs = {
    //     let mut elifs = vec![];
    //     for elif_expr in &mut *if_expr.elif {
    //         match &mut *elif_expr {
    //             Some(inner_if_expr) => elifs
    //                 .push(generate_if(inner_if_expr, ident_tracker, do_await, in_static).unwrap()),
    //             None => unreachable!(),
    //         }
    //     }
    //     elifs
    // };
    Ok(())
}

pub(super) fn generate_for(for_expr: &For, tracker: &ValueTracker) -> IResult<()> {
    Ok(())
}

pub(super) fn generate_fn_call(fn_call_expr: &FnCall, tracker: &ValueTracker) -> IResult<()> {
    // Before generating functions, check function table to check whether it's properly defined or not
    Ok(())
}

pub(super) fn generate_fn_call_arg(
    fn_call_arg: &FnCallArgs,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<RuLaValue> {
    Ok(RuLaValue::Str(String::from("")))
}

pub(super) fn generate_rule_call(rule_call_expr: &RuleCall, tracker: &ValueTracker) -> IResult<()> {
    // 0. check the rule name if the definition exists
    let rule_name = &*rule_call_expr.rule_name.name;
    if !tracker.borrow().check_rule_name_exist(rule_name) {
        return Err(RuleSetGenError::NoRuleFoundError);
    }

    let scope = rule_name;
    // At this point, repeater argument and ordinary argument should be resolved
    // Currently the number of repater for one rule is limited to 1
    let repeater_index =
        generate_rep_call_arg(&*rule_call_expr.repeater_arg, tracker, scope).unwrap();
    let repeater_arg = &tracker.borrow().repeaters[repeater_index];
    let mut arguments = Arguments::place_holder();
    arguments.update_scope(scope);
    for arg in &rule_call_expr.argument {
        arguments.add_val(generate_fn_call_arg(arg, tracker, scope).unwrap().clone());
    }

    // Check argument types, argument numbers, values
    // let stage = rule_evaluator(rule_name, repeater_arg, &arguments, tracker).unwrap();

    let stage = tracker
        .borrow()
        .eval_rule(rule_name, repeater_arg, &arguments, tracker, scope);

    // Add stage to the ruleset
    // At this moment, repeater index must be corresponding to where this rule is added
    tracker.borrow().add_stage(repeater_index, stage);

    // Add rule to stage inside the RuleSet
    Ok(())
}

// Repeater call must be casted to usize to
pub(super) fn generate_rep_call_arg(
    repeater_call_arg: &RepeaterCallArg,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<usize> {
    let index: usize = match repeater_call_arg {
        RepeaterCallArg::Term(term) => 0,
        RepeaterCallArg::Ident(ident) => 0,
        RepeaterCallArg::NumberLit(number_lit) => {
            let val = &*number_lit.value.name.clone();
            val.parse::<usize>().unwrap()
        }
        RepeaterCallArg::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    };
    Ok(index as usize)
}

// Promote resources to the next stage
pub(super) fn generate_return(return_expr: &Return, tracker: &ValueTracker) -> IResult<()> {
    Ok(())
}

pub(super) fn generate_send(send_expr: &Send, tracker: &ValueTracker) -> IResult<()> {
    Ok(())
}

// Generate Match expression to achieve match action
// In the case of static generation, this expand Rules to the stages
pub(super) fn generate_match(match_expr: &Match, tracker: &ValueTracker) -> IResult<()> {
    Ok(())
}

pub(super) fn generate_match_arm(match_arm: &MatchArm, tracker: &ValueTracker) -> IResult<()> {
    // let generated_match_condition =
    //     generate_match_condition(&mut *match_arm.condition, ident_tracker).unwrap();
    // let generated_match_action =
    //     generate_match_action(&mut *match_arm.action, ident_tracker, false, false).unwrap();
    // Ok(quote!(#generated_match_condition => {#generated_match_action}))
    Ok(())
}

pub(super) fn generate_match_condition(
    match_condition: &MatchCondition,
    tracker: &ValueTracker,
) -> IResult<()> {
    // Right now, satisfiable can only take literals, but in the future, this should be more flexible
    match &*match_condition.satisfiable {
        Satisfiable::Lit(literal) => Ok(generate_lit(literal, tracker).unwrap()),
        _ => Err(RuleSetGenError::InitializationError),
    }
}

pub(super) fn generate_match_action(
    match_action: &MatchAction,
    ident_tracker: &ValueTracker,
) -> IResult<()> {
    // let mut actionables = vec![];
    // for actionable in &mut *match_action.actionable {
    //     actionables.push(
    //         generate_expr(
    //             actionable,
    //             None,
    //             ident_tracker,
    //             true,
    //             in_static,
    //             in_closure,
    //             false,
    //         )
    //         .unwrap(),
    //     );
    // }
    // Ok(quote!(
    //     #(#actionables);*
    // ))
    Ok(())
}

pub(super) fn generate_comp(
    comp_expr: &Comp,
    tracker: &ValueTracker,
    scope: &Scope,
) -> IResult<()> {
    let lhs = generate_expr(&comp_expr.lhs, tracker, scope).unwrap();
    let rhs = generate_expr(&comp_expr.rhs, tracker, scope).unwrap();
    let (op, cmp_op) = match *comp_expr.comp_op {
        CompOpKind::Lt => (quote!(<), quote!(__CmpOp::Lt)),
        CompOpKind::Gt => (quote!(>), quote!(__CmpOp::Gt)),
        CompOpKind::LtE => (quote!(<=), quote!(__CmpOp::LtE)),
        CompOpKind::GtE => (quote!(>=), quote!(__CmpOp::GtE)),
        CompOpKind::Eq => (quote!(==), quote!(__CmpOp::Eq)),
        CompOpKind::Nq => (quote!(!=), quote!(__CmpOp::Nq)),
        CompOpKind::PlaceHolder => return Err(RuleSetGenError::InitializationError),
    };
    Ok(())
}

pub(super) fn generate_variable_call(
    variable_call_expr: &VariableCallExpr,
    tracker: &ValueTracker,
) -> IResult<()> {
    Ok(())
}

pub(super) fn generate_lit(lit: &Lit, tracker: &ValueTracker) -> IResult<()> {
    match &*lit.kind {
        LitKind::Ident(ident_lit) => generate_ident(ident_lit, tracker).unwrap(),
        LitKind::NumberLit(number_lit) => {
            number_lit.value.name.parse::<i64>();
        }
        LitKind::StringLit(string_lit) => {
            // SynLit::Str(LitStr::new(&string_lit.as_string(), Span::call_site()));
        }
        _ => todo!("{:#?} not yet implemented", lit),
    }
    Ok(())
}

pub(super) fn generate_term(term_expr: &Term, tracker: &ValueTracker) -> IResult<()> {
    // We could make Term struct instead of direct calc
    // For now, this function just returns calc result as f64
    // let val = LitFloat::new(&term_expr.to_string(), Span::call_site());
    // Ok(quote!(#val))
    Ok(())
}

// Generate identifier token from Ident ast
pub(super) fn generate_ident(ident: &Ident, tracker: &ValueTracker) -> IResult<()> {
    Ok(())
}

pub(super) fn generate_type_hint(type_hint: &TypeDef) -> IResult<Types> {
    match type_hint {
        TypeDef::Repeater => Ok(Types::Repeater),
        TypeDef::Boolean => Ok(Types::Boolean),
        TypeDef::Integer => Ok(Types::Int),
        TypeDef::UnsignedInteger => Ok(Types::UInt),
        TypeDef::Float => Ok(Types::Float),
        TypeDef::Str => Ok(Types::Str),
        TypeDef::Qubit => Ok(Types::Qubit),
        TypeDef::Vector(inner) => {
            let inner_type = generate_type_hint(inner).unwrap();
            Ok(Types::Vec(Box::new(inner_type)))
        }
        TypeDef::PlaceHolder => return Err(RuleSetGenError::InitializationError),
        _ => todo!("type {:#?} not yet implemented", type_hint),
    }
}
