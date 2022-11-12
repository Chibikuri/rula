pub mod ast;
pub mod error;
mod util;

use crate::Rule;
// RuLa
use ast::*;
use error::RuLaError;
use std::path::PathBuf;

use once_cell::sync::Lazy;
use pest::iterators::Pair;
use pest::prec_climber::{Assoc, Operator, PrecClimber};

// Custome error interface for rula
pub type IResult<T> = std::result::Result<T, RuLaError>;

static PREC_CLIMBER: Lazy<PrecClimber<Rule>> = Lazy::new(|| {
    PrecClimber::new(vec![
        Operator::new(Rule::plus, Assoc::Left) | Operator::new(Rule::minus, Assoc::Left),
        Operator::new(Rule::asterisk, Assoc::Left) | Operator::new(Rule::slash, Assoc::Left),
        Operator::new(Rule::caret, Assoc::Right),
    ])
});

/**
 * parse rula system and if it matches program, going to `build_ast_from_program`.
*/
pub fn build_ast_from_rula(pair: Pair<Rule>) -> IResult<RuLa> {
    match pair.as_rule() {
        Rule::program => {
            if pair.as_str() == "" {
                // edge case (e.g. only comment)
                return Ok(RuLa::ignore());
            } else {
                return Ok(RuLa::new(RuLaKind::Program(
                    build_ast_from_program(pair).unwrap(),
                )));
            }
        }
        Rule::EOI => Ok(RuLa::aoi()),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

// Parse program <-> statement
fn build_ast_from_program(pair: Pair<Rule>) -> IResult<Program> {
    let mut program = Program::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::stmt => program.add_program(ProgramKind::Stmt(
                build_ast_from_stmt(block.into_inner().next().unwrap()).unwrap(),
            )),
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(program)
}

fn build_ast_from_interface(pair: Pair<Rule>) -> IResult<Interface> {
    let mut interface = Interface::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::ident_list => {
                // interface list
                for interface_name in block.into_inner() {
                    let interface_ident = build_ast_from_ident(interface_name).unwrap();
                    interface.add_interface(interface_ident);
                }
            }
            Rule::ident => {
                // group name
                let interface_group = build_ast_from_ident(block).unwrap();
                interface.add_name(Some(interface_group));
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }

    Ok(interface)
}

// Parse statement <--> {Let statement | expression}
fn build_ast_from_stmt(pair: Pair<Rule>) -> IResult<Stmt> {
    match pair.as_rule() {
        Rule::config_def => Ok(Stmt::new(StmtKind::Config(
            build_ast_from_config_def(pair).unwrap(),
        ))),
        Rule::interface_def => Ok(Stmt::new(StmtKind::Interface(
            build_ast_from_interface(pair).unwrap(),
        ))),
        Rule::let_stmt => Ok(Stmt::new(StmtKind::Let(
            build_ast_from_let_stmt(pair).unwrap(),
        ))),
        Rule::expr => Ok(Stmt::new(StmtKind::Expr(
            build_ast_from_expr(pair.into_inner().next().unwrap()).unwrap(),
        ))),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

fn build_ast_from_config_def(pair: Pair<Rule>) -> IResult<Config> {
    let mut config_def = Config::place_holder();
    for config_block in pair.into_inner() {
        match config_block.as_rule() {
            Rule::config_item => {
                config_def.add_value(build_ast_from_config_item(config_block).unwrap());
            }
            Rule::ident => {
                config_def.update_config_name(build_ast_from_ident(config_block).unwrap());
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(config_def)
}

fn build_ast_from_config_item(pair: Pair<Rule>) -> IResult<ConfigItem> {
    let mut config_item = ConfigItem::place_holder();
    for block in pair.into_inner() {
        println!("block {:#?}", block);
        match block.as_rule() {
            Rule::ident => {
                config_item.update_name(build_ast_from_ident(block).unwrap());
            }
            Rule::typedef_lit => {
                config_item.add_type_def(
                    build_ast_from_typedef_lit(block.into_inner().next().unwrap()).unwrap(),
                );
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(config_item)
}
// Parse Let statement (semi endpoint)
fn build_ast_from_let_stmt(pair: Pair<Rule>) -> IResult<Let> {
    let mut let_stmt = Let::place_holder();
    for let_pair in pair.into_inner() {
        match let_pair.as_rule() {
            Rule::ident => {
                let_stmt.add_ident(build_ast_from_ident(let_pair).unwrap());
            }
            Rule::ident_typed => {
                let_stmt.add_ident(build_ast_from_ident_typed(let_pair).unwrap());
            }
            Rule::expr => {
                let_stmt
                    .add_expr(build_ast_from_expr(let_pair.into_inner().next().unwrap()).unwrap());
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(let_stmt)
}

fn build_ast_from_ident_typed(pair: Pair<Rule>) -> IResult<Ident> {
    let mut identifier = Ident::place_holder();
    for ids in pair.into_inner() {
        match ids.as_rule() {
            Rule::ident => {
                identifier = build_ast_from_ident(ids).unwrap();
            }
            Rule::typedef_lit => {
                identifier.add_type_hint(Some(
                    build_ast_from_typedef_lit(ids.into_inner().next().unwrap()).unwrap(),
                ));
            }
            _ => unreachable!(),
        }
    }
    Ok(identifier)
}

// Parse identifier(Endpoint)
fn build_ast_from_ident(pair: Pair<Rule>) -> IResult<Ident> {
    let mut identifier = Ident::place_holder();
    match pair.as_rule() {
        Rule::ident => {
            identifier.add_name(pair.as_str());
        }
        _ => return Err(RuLaError::RuLaSyntaxError),
    }
    Ok(identifier)
}

// Parse expression <--> {If | Import | Lit | Identifier}
fn build_ast_from_expr(pair: Pair<Rule>) -> IResult<Expr> {
    match pair.as_rule() {
        // Same order as .pest file
        // import hello::world;
        Rule::import_expr => Ok(Expr::new(ExprKind::Import(
            build_ast_from_import_expr(pair).unwrap(),
        ))),
        // fn(hello:i32){expression;};
        Rule::fn_def_expr => Ok(Expr::new(ExprKind::FnDef(
            build_ast_from_fn_def_expr(pair).unwrap(),
        ))),
        // if(block){expression;};
        Rule::if_expr => Ok(Expr::new(ExprKind::If(
            build_ast_from_if_expr(pair).unwrap(),
        ))),
        Rule::for_expr => Ok(Expr::new(ExprKind::For(
            build_ast_from_for_expr(pair).unwrap(),
        ))),
        Rule::while_expr => Ok(Expr::new(ExprKind::While(
            build_ast_from_while_expr(pair).unwrap(),
        ))),
        Rule::struct_expr => Ok(Expr::new(ExprKind::Struct(
            buil_ast_from_struct_expr(pair).unwrap(),
        ))),
        Rule::comp_expr => Ok(Expr::new(ExprKind::Comp(
            build_ast_from_comp_expr(pair).unwrap(),
        ))),
        Rule::return_expr => Ok(Expr::new(ExprKind::Return(
            build_ast_from_return_expr(pair).unwrap(),
        ))),
        Rule::match_expr => Ok(Expr::new(ExprKind::Match(
            build_ast_from_match_expr(pair).unwrap(),
        ))),
        Rule::ruleset_expr => Ok(Expr::new(ExprKind::RuleSetExpr(
            build_ast_from_ruleset_expr(pair).unwrap(),
        ))),
        Rule::rule_expr => Ok(Expr::new(ExprKind::RuleExpr(
            build_ast_from_rule_expr(pair).unwrap(),
        ))),
        Rule::cond_expr => Ok(Expr::new(ExprKind::CondExpr(
            build_ast_from_cond_expr(pair).unwrap(),
        ))),
        Rule::act_expr => Ok(Expr::new(ExprKind::ActExpr(
            build_ast_from_act_expr(pair).unwrap(),
        ))),
        Rule::fn_call_expr => Ok(Expr::new(ExprKind::FnCall(
            build_ast_from_fn_call_expr(pair).unwrap(),
        ))),
        Rule::variable_call_expr => Ok(Expr::new(ExprKind::VariableCallExpr(
            build_ast_from_variable_call_expr(pair).unwrap(),
        ))),
        Rule::braket_expr => Ok(Expr::new(ExprKind::Array(
            build_ast_from_braket_expr(pair).unwrap(),
        ))),
        // 1+10, (1+18)*20
        // Rule::term => Ok(Expr::new(ExprKind::Term(eval_term(pair.into_inner())))),
        Rule::term_expr => Ok(Expr::new(ExprKind::Term(eval_prec(pair)))),
        // true, false, "words"
        Rule::literal_expr => Ok(Expr::new(ExprKind::Lit(
            build_ast_from_literals(pair.into_inner().next().unwrap()).unwrap(),
        ))),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

fn build_ast_from_variable_call_expr(pair: Pair<Rule>) -> IResult<VariableCallExpr> {
    let mut variable_call = VariableCallExpr::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::callable => {
                variable_call.add_variable(
                    build_ast_from_callable(block.into_inner().next().unwrap()).unwrap(),
                );
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(variable_call)
}

fn build_ast_from_callable(pair: Pair<Rule>) -> IResult<Callable> {
    match pair.as_rule() {
        Rule::ident => Ok(Callable::Ident(build_ast_from_ident(pair).unwrap())),
        Rule::fn_call_expr => Ok(Callable::FnCall(build_ast_from_fn_call_expr(pair).unwrap())),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

// Don't want to evaluate anything at this moment.
fn eval_prec(pair: Pair<Rule>) -> f64 {
    // primary closure taking pair and throw it to consume function.
    let primary = |pair| eval_prec(pair);
    let infix = |lhs: f64, op: Pair<Rule>, rhs: f64| match op.as_rule() {
        Rule::plus => lhs + rhs,
        Rule::minus => lhs - rhs,
        Rule::asterisk => lhs * rhs,
        Rule::slash => lhs / rhs,
        Rule::caret => lhs.powf(rhs),
        _ => unreachable!("operation unreachable, {:#?}", &op),
    };

    match pair.as_rule() {
        Rule::term_expr => PREC_CLIMBER.climb(pair.into_inner(), primary, infix),
        Rule::number => pair.as_str().parse().unwrap(),
        _ => unreachable!("unreachable{:#?}", &pair),
    }
}

// Parse Literals <--> {string literal | boolean literal}
fn build_ast_from_literals(pair: Pair<Rule>) -> IResult<Lit> {
    match pair.as_rule() {
        // identifier
        Rule::ident => Ok(Lit::new(LitKind::Ident(
            build_ast_from_ident(pair).unwrap(),
        ))),
        Rule::raw_string => Ok(Lit::new(LitKind::StringLit(StringLit::new(pair.as_str())))),
        Rule::number => Ok(Lit::new(LitKind::NumberLit(NumberLit::new(pair.as_str())))),
        Rule::binary => Ok(Lit::new(LitKind::BinaryLit(BinaryLit::new(
            pair.into_inner().next().unwrap().as_str(),
        )))),
        Rule::hex => Ok(Lit::new(LitKind::HexLit(HexLit::new(
            pair.into_inner().next().unwrap().as_str(),
        )))),
        Rule::unicord => Ok(Lit::new(LitKind::UnicordLit(UnicordLit::new(
            pair.into_inner().next().unwrap().as_str(),
        )))),
        Rule::bool => match pair.as_str() {
            "true" => Ok(Lit::new(LitKind::BooleanLit(true))),
            "false" => Ok(Lit::new(LitKind::BooleanLit(false))),
            _ => unreachable!(),
        },
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

// Parse If expr <--> {paren_expr | brace_stmt | else_if | else} semi endpoint
fn build_ast_from_if_expr(pair: Pair<Rule>) -> IResult<If> {
    // `pair` structure
    // if_expr -> inner {paren_expr, block_expr}
    let mut if_expr = If::place_holder(); // initialize if expr
    for expr in pair.into_inner() {
        match expr.as_rule() {
            // block statement
            Rule::expr => {
                if_expr.add_block(build_ast_from_expr(expr.into_inner().next().unwrap()).unwrap());
            }
            Rule::stmt => {
                // nested expr (brace stmt -> stmt)
                if_expr.add_stmt(build_ast_from_stmt(expr.into_inner().next().unwrap()).unwrap());
            }
            Rule::else_if_expr => {
                // recursively apply
                if_expr.add_elif(build_ast_from_if_expr(expr).unwrap());
            }
            Rule::else_expr => {
                let else_stmt = build_ast_from_stmt(
                    expr.into_inner()
                        .next()
                        .unwrap()
                        .into_inner()
                        .next()
                        .unwrap(),
                )
                .unwrap();
                if_expr.add_else(else_stmt);
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    // Check if block and expresions are properly set
    if_expr.check();
    Ok(if_expr)
}

fn build_ast_from_for_expr(pair: Pair<Rule>) -> IResult<For> {
    let mut for_expr = For::place_holder();
    // for_expr = { ^"for" ~ "(" ~ pattern ~")"~ "in" ~ generator ~ brace_stmt }
    for blocks in pair.into_inner() {
        match blocks.as_rule() {
            Rule::ident_list => {
                // omitted (patterns)
                // for identifier lists (ident, ident2, ...)
                for ident in blocks.into_inner() {
                    // get ident
                    for_expr.add_ident(build_ast_from_ident(ident).unwrap());
                }
            }
            Rule::generator => {
                // generator can get three expressions right now.
                let gen_expression = blocks.into_inner().next().unwrap();
                let expr = match gen_expression.as_rule() {
                    // { braket_expr | fn_call_expr | literal_expr }
                    Rule::braket_expr => Expr::new(ExprKind::Array(
                        build_ast_from_braket_expr(gen_expression.into_inner().next().unwrap())
                            .unwrap(),
                    )),
                    Rule::fn_call_expr => {
                        // fn call e.g. hello_world()
                        Expr::new(ExprKind::FnCall(
                            build_ast_from_fn_call_expr(gen_expression).unwrap(),
                        ))
                    }
                    Rule::literal_expr => {
                        // Only identifier can go here
                        Expr::new(ExprKind::Lit(
                            build_ast_from_literals(gen_expression.into_inner().next().unwrap())
                                .unwrap(),
                        ))
                    }
                    _ => unreachable!(),
                };
                for_expr.add_expr(expr);
            }
            Rule::stmt => {
                for_expr
                    .add_stmt(build_ast_from_stmt(blocks.into_inner().next().unwrap()).unwrap());
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(for_expr)
}

fn build_ast_from_while_expr(pair: Pair<Rule>) -> IResult<While> {
    let mut while_expr = While::place_holder();
    for blocks in pair.into_inner() {
        match blocks.as_rule() {
            Rule::expr => while_expr
                .add_block(build_ast_from_expr(blocks.into_inner().next().unwrap()).unwrap()),
            Rule::stmt => while_expr
                .add_stmt(build_ast_from_stmt(blocks.into_inner().next().unwrap()).unwrap()),
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(while_expr)
}

fn buil_ast_from_struct_expr(pair: Pair<Rule>) -> IResult<Struct> {
    let mut struct_expr = Struct::place_holder();
    for st in pair.into_inner() {
        match st.as_rule() {
            Rule::struct_name => {
                struct_expr.add_name(build_ast_from_ident(st.into_inner().next().unwrap()).unwrap())
            }
            Rule::ident_typed => struct_expr.add_item(build_ast_from_ident_typed(st).unwrap()),
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(struct_expr)
}

fn build_ast_from_return_expr(pair: Pair<Rule>) -> IResult<Return> {
    let mut return_expr = Return::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::ident => return_expr.add_target(Expr::new(ExprKind::Lit(Lit::new(
                LitKind::Ident(build_ast_from_ident(block).unwrap()),
            )))),
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(return_expr)
}

fn build_ast_from_match_expr(pair: Pair<Rule>) -> IResult<Match> {
    let mut match_expr = Match::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::ident => match_expr.add_temp_val(Some(build_ast_from_ident(block).unwrap())),
            Rule::expr => match_expr
                .add_expr(build_ast_from_expr(block.into_inner().next().unwrap()).unwrap()),
            Rule::match_arm => match_expr.add_match_arm(build_ast_from_match_arm(block).unwrap()),
            // Here this can only be finally:
            Rule::match_action => {
                match_expr.add_finally(Some(build_ast_from_match_action(block).unwrap()))
            }
            // Rule::ident => match_expr.
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(match_expr)
}

fn build_ast_from_match_arm(pair: Pair<Rule>) -> IResult<MatchArm> {
    let mut match_arm = MatchArm::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::match_condition => match_arm.add_condition(
                build_ast_from_match_condition(block.into_inner().next().unwrap()).unwrap(),
            ),
            Rule::match_action => match_arm.add_action(build_ast_from_match_action(block).unwrap()),
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(match_arm)
}

fn build_ast_from_match_condition(pair: Pair<Rule>) -> IResult<MatchCondition> {
    let mut match_condition = MatchCondition::place_holder();
    match pair.as_rule() {
        Rule::satisfiable => match_condition.update_satisfiable(
            build_ast_from_satisfiable(pair.into_inner().next().unwrap()).unwrap(),
        ),
        _ => return Err(RuLaError::RuLaSyntaxError),
    }
    Ok(match_condition)
}

fn build_ast_from_satisfiable(pair: Pair<Rule>) -> IResult<Satisfiable> {
    match pair.as_rule() {
        // Rule::comp_expr => Ok(Satisfiable::Comp(build_ast_from_comp_expr(pair).unwrap())),
        // Rule::ident => Ok(Satisfiable::Ident(build_ast_from_ident(pair).unwrap())),
        Rule::literal_expr => Ok(Satisfiable::Lit(
            build_ast_from_literals(pair.into_inner().next().unwrap()).unwrap(),
        )),
        _ => return Err(RuLaError::RuLaSyntaxError),
    }
}

fn build_ast_from_match_action(pair: Pair<Rule>) -> IResult<MatchAction> {
    let mut match_action = MatchAction::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::expr => match_action
                .add_actionable(build_ast_from_expr(block.into_inner().next().unwrap()).unwrap()),
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(match_action)
}

fn build_ast_from_comp_expr(pair: Pair<Rule>) -> IResult<Comp> {
    let mut comp_op = CompOpKind::PlaceHolder;
    let mut comp_expr = Comp::place_holder();
    let mut expressions = vec![];
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::comparable => {
                expressions.push(build_ast_from_expr(block.into_inner().next().unwrap()).unwrap());
            }
            Rule::comp_op => match block.as_str() {
                "<" => comp_op = CompOpKind::Lt,
                ">" => comp_op = CompOpKind::Gt,
                "=<" => comp_op = CompOpKind::LtE,
                ">=" => comp_op = CompOpKind::GtE,
                "==" => comp_op = CompOpKind::Eq,
                "!=" => comp_op = CompOpKind::Nq,
                _ => unreachable!("Unknown ops!"),
            },
            _ => unreachable!(),
        }
    }
    if expressions.len() != 2 {
        // Should this be avoided by grammar level?
        return Err(RuLaError::RuLaSyntaxError);
    }
    comp_expr.add_lhs(expressions[0].clone());
    comp_expr.add_comp_op(comp_op);
    comp_expr.add_rhs(expressions[1].clone());
    Ok(comp_expr)
}

fn build_ast_from_ruleset_expr(pair: Pair<Rule>) -> IResult<RuleSetExpr> {
    let mut ruleset_expr = RuleSetExpr::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::ruleset_config => {
                ruleset_expr.add_config_name(Some(build_ast_from_ident(block.into_inner().next().unwrap()).unwrap()));
            }
            Rule::ident => {
                ruleset_expr.add_name(build_ast_from_ident(block).unwrap());
            }
            Rule::fn_call_expr => {
                ruleset_expr.add_default(Some(build_ast_from_fn_call_expr(block).unwrap()));
            }
            Rule::rule_idents => {
                ruleset_expr.add_rule(
                    build_ast_from_rule_idents(block.into_inner().next().unwrap()).unwrap(),
                );
            }
            _ => unreachable!(),
        }
    }
    Ok(ruleset_expr)
}

fn build_ast_from_rule_idents(pair: Pair<Rule>) -> IResult<RuleIdentifier> {
    match pair.as_rule() {
        Rule::fn_call_expr => Ok(RuleIdentifier::FnCall(
            build_ast_from_fn_call_expr(pair).unwrap(),
        )),
        Rule::let_stmt => Ok(RuleIdentifier::Let(build_ast_from_let_stmt(pair).unwrap())),
        _ => unreachable!(),
    }
}

fn build_ast_from_rule_expr(pair: Pair<Rule>) -> IResult<RuleExpr> {
    let mut rule_expr = RuleExpr::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::ident => {
                // rule_name
                rule_expr.add_name(build_ast_from_ident(block).unwrap());
            }
            Rule::ident_list => {
                for interface in block.into_inner() {
                    let interface_ident = build_ast_from_ident(interface).unwrap();
                    // Interface names
                    rule_expr.add_interface(interface_ident).unwrap();
                }
            }
            Rule::argument_def => {
                for arg in block.into_inner() {
                    match arg.as_rule() {
                        Rule::ident_typed => {
                            rule_expr.add_arg(build_ast_from_ident_typed(arg).unwrap())
                        }
                        Rule::ident => rule_expr.add_arg(build_ast_from_ident(arg).unwrap()),
                        _ => return Err(RuLaError::RuLaSyntaxError),
                    }
                }
            }
            Rule::rule_contents => {
                rule_expr.add_rule_content(build_ast_from_rule_contents(block).unwrap())
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(rule_expr)
}

fn build_ast_from_rule_contents(pair: Pair<Rule>) -> IResult<RuleContentExpr> {
    let mut rule_content_expr = RuleContentExpr::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::cond_expr => {
                rule_content_expr.add_condition_expr(build_ast_from_cond_expr(block).unwrap())
            }
            Rule::act_expr => {
                rule_content_expr.add_action_expr(build_ast_from_act_expr(block).unwrap())
            }
            Rule::stmt => rule_content_expr
                .add_post_stmt(build_ast_from_stmt(block.into_inner().next().unwrap()).unwrap()),
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(rule_content_expr)
}

fn build_ast_from_monitor_expr(pair: Pair<Rule>) -> IResult<Option<WatchExpr>> {
    let mut monitor_expr = WatchExpr::place_holder();
    for let_stmt in pair.into_inner() {
        let watched = build_ast_from_let_stmt(let_stmt).unwrap();
        monitor_expr.add_watch_value(watched);
    }
    Ok(Some(monitor_expr))
}

fn build_ast_from_cond_expr(pair: Pair<Rule>) -> IResult<CondExpr> {
    let mut cond_expr = CondExpr::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::ident => cond_expr.add_name(Some(build_ast_from_ident(block).unwrap())),
            Rule::monitor_expr => {
                cond_expr.add_watch_expr(build_ast_from_monitor_expr(block).unwrap())
            }
            Rule::awaitable => cond_expr.add_awaitable(
                build_ast_from_awaitable_expr(block.into_inner().next().unwrap()).unwrap(),
            ),
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(cond_expr)
}

fn build_ast_from_awaitable_expr(pair: Pair<Rule>) -> IResult<Awaitable> {
    match pair.as_rule() {
        Rule::fn_call_expr => Ok(Awaitable::FnCall(
            build_ast_from_fn_call_expr(pair).unwrap(),
        )),
        Rule::variable_call_expr => Ok(Awaitable::VariableCallExpr(
            build_ast_from_variable_call_expr(pair).unwrap(),
        )),
        Rule::comp_expr => Ok(Awaitable::Comp(build_ast_from_comp_expr(pair).unwrap())),
        _ => unreachable!("No more awaitable conditions allowed"),
    }
}

fn build_ast_from_act_expr(pair: Pair<Rule>) -> IResult<ActExpr> {
    let mut act_expr = ActExpr::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::ident => act_expr.add_name(Some(build_ast_from_ident(block).unwrap())),
            Rule::stmt => act_expr
                .add_operatable(build_ast_from_stmt(block.into_inner().next().unwrap()).unwrap()),
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(act_expr)
}
fn build_ast_from_import_expr(pair: Pair<Rule>) -> IResult<Import> {
    let mut path_list: Vec<PathBuf> = vec![];
    let mut end_paths: Vec<&str> = vec![];
    // ::{a, b} expression must be single not two of them
    // e.g. not allowed ::{a, b}::{c, d}
    // This can be rejected by level of grammar
    let mut path = PathBuf::new();
    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::ident => path.push(inner_pair.as_str()),
            Rule::ident_list => {
                for inner_inner_pair in inner_pair.into_inner() {
                    end_paths.push(inner_inner_pair.as_str());
                }
                // Path must be completed once this is called.
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    // No end component such as `hello::{a, b}`
    // then just add `hello` as a path
    if end_paths.len() == 0 {
        path_list.push(path)
    } else {
        for t_path in end_paths {
            let mut cloned_path = path.clone();
            cloned_path.push(t_path);
            path_list.push(cloned_path);
        }
    }

    Ok(Import::new(PathKind::from(path_list)))
}

fn build_ast_from_fn_def_expr(pair: Pair<Rule>) -> IResult<FnDef> {
    let mut fnc_def = FnDef::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::argument_def => {
                // loop over all arguments
                for arg in block.into_inner() {
                    fnc_def.add_arg(build_ast_from_ident_typed(arg).unwrap());
                }
            }
            Rule::stmt => {
                fnc_def.add_expr(build_ast_from_stmt(block.into_inner().next().unwrap()).unwrap())
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(fnc_def)
}

fn build_ast_from_fn_call_expr(pair: Pair<Rule>) -> IResult<FnCall> {
    let mut fnc_call = FnCall::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::ident => {
                fnc_call.add_name(build_ast_from_ident(block).unwrap());
            }
            Rule::expr => {
                for arg in block.into_inner() {
                    fnc_call.add_argument(build_ast_from_expr(arg).unwrap());
                }
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(fnc_call)
}

fn build_ast_from_braket_expr(pair: Pair<Rule>) -> IResult<Array> {
    let mut array = Array::place_holder();
    for lit in pair.into_inner() {
        array.add_item(build_ast_from_literals(lit.into_inner().next().unwrap()).unwrap())
    }
    Ok(array)
}

fn build_ast_from_typedef_lit(pair: Pair<Rule>) -> IResult<TypeDef> {
    match pair.as_rule() {
        Rule::integer_type => match pair.as_str() {
            "i32" => return Ok(TypeDef::Integer32),
            "i64" => return Ok(TypeDef::Integer64),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        Rule::unsigned_integer_type => match pair.as_str() {
            "u32" => return Ok(TypeDef::UnsignedInteger32),
            "u64" => return Ok(TypeDef::UnsignedInteger64),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        Rule::float_type => match pair.as_str() {
            "f32" => return Ok(TypeDef::Float32),
            "f64" => return Ok(TypeDef::Float64),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        Rule::complex_type => match pair.as_str() {
            "c64" => return Ok(TypeDef::Complex64),
            "c128" => return Ok(TypeDef::Complex128),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        Rule::boolean_type => match pair.as_str() {
            "bool" => return Ok(TypeDef::Boolean),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        Rule::string_type => match pair.as_str() {
            "str" => return Ok(TypeDef::Str),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        Rule::qubit_type => match pair.as_str() {
            "Qubit" => return Ok(TypeDef::Qubit),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        Rule::vector_type => Ok(TypeDef::Vector(Box::new(
            build_ast_from_typedef_lit(
                pair.into_inner()
                    .next()
                    .unwrap()
                    .into_inner()
                    .next()
                    .unwrap(),
            )
            .unwrap(),
        ))),
        _ => todo!("Should be unknown type {:#?} error here", pair),
    }
}
