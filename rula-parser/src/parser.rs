pub mod ast;
pub mod error;
mod util;

use std::env;

use crate::{parse, Rule};
// RuLa
use ast::*;
use error::RuLaError;
use std::{fs::File, io::Read, path::PathBuf};

// use once_cell::sync::Lazy;
use pest::iterators::Pair;
// use pest::prec_climber::{Assoc, Operator, PrecClimber};

// Custome error interface for rula
pub type IResult<T> = std::result::Result<T, RuLaError>;

// static PREC_CLIMBER: Lazy<PrecClimber<Rule>> = Lazy::new(|| {
//     PrecClimber::new(vec![
//         Operator::new(Rule::plus, Assoc::Left) | Operator::new(Rule::minus, Assoc::Left),
//         Operator::new(Rule::asterisk, Assoc::Left) | Operator::new(Rule::slash, Assoc::Left),
//         Operator::new(Rule::caret, Assoc::Right),
//     ])
// });

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
        Rule::EOI => Ok(RuLa::eoi()),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

// Parse program <-> statement
fn build_ast_from_program(pair: Pair<Rule>) -> IResult<Program> {
    let mut program = Program::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::repeaters => program.add_program(ProgramKind::Repeaters),
            Rule::import_stmt => program.add_program(ProgramKind::Import(
                build_ast_from_import_stmt(block).unwrap(),
            )),
            Rule::ruleset_stmt => program.add_program(ProgramKind::RuleSetExpr(
                build_ast_from_ruleset_stmt(block).unwrap(),
            )),
            Rule::rule_stmt => program.add_program(ProgramKind::RuleExpr(
                build_ast_from_rule_stmt(block).unwrap(),
            )),
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(program)
}

// Parse statement <--> {Let statement | expression}
fn build_ast_from_stmt(pair: Pair<Rule>) -> IResult<Stmt> {
    match pair.as_rule() {
        Rule::let_stmt => Ok(Stmt::new(StmtKind::Let(
            build_ast_from_let_stmt(pair).unwrap(),
        ))),
        Rule::if_stmt => Ok(Stmt::new(StmtKind::If(
            build_ast_from_if_stmt(pair).unwrap(),
        ))),
        Rule::for_stmt => Ok(Stmt::new(StmtKind::For(
            build_ast_from_for_stmt(pair).unwrap(),
        ))),
        Rule::match_stmt => Ok(Stmt::new(StmtKind::Match(
            build_ast_from_match_stmt(pair).unwrap(),
        ))),
        Rule::promote_stmt => Ok(Stmt::new(StmtKind::Promote(
            build_ast_from_promote_stmt(pair).unwrap(),
        ))),
        Rule::send_stmt => Ok(Stmt::new(StmtKind::Send(
            build_ast_from_send_stmt(pair).unwrap(),
        ))),
        Rule::set_stmt => Ok(Stmt::new(StmtKind::Set(
            build_ast_from_set_stmt(pair).unwrap(),
        ))),
        Rule::expr => Ok(Stmt::new(StmtKind::Expr(
            build_ast_from_expr(pair.into_inner().next().unwrap()).unwrap(),
        ))),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
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
        Rule::get_expr => Ok(Expr::new(ExprKind::Get(
            build_ast_from_get_expr(pair).unwrap(),
        ))),
        Rule::fn_call_expr => Ok(Expr::new(ExprKind::FnCall(
            build_ast_from_fn_call_expr(pair).unwrap(),
        ))),
        Rule::rule_call_expr => Ok(Expr::new(ExprKind::RuleCall(
            build_ast_from_rule_call_expr(pair).unwrap(),
        ))),
        Rule::vector => Ok(Expr::new(ExprKind::RuLaVec(
            build_ast_from_vector(pair).unwrap(),
        ))),
        Rule::tuple => Ok(Expr::new(ExprKind::RuLaTuple(
            build_ast_from_tuple(pair).unwrap(),
        ))),
        Rule::comp_expr => Ok(Expr::new(ExprKind::Comp(
            build_ast_from_comp_expr(pair).unwrap(),
        ))),
        Rule::term_expr => Ok(Expr::new(ExprKind::Term(
            build_ast_from_term_expr(pair).unwrap(),
        ))),
        Rule::variable_call_expr => Ok(Expr::new(ExprKind::VariableCallExpr(
            build_ast_from_variable_call_expr(pair).unwrap(),
        ))),
        Rule::literal_expr => Ok(Expr::new(ExprKind::Lit(
            build_ast_from_literals(pair.into_inner().next().unwrap()).unwrap(),
        ))),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

// Two possible function importing
// import hello::world ... single import
// import hello::{world, me} ... multiple import
// Grammar for the import statement
// import_stmt = { ^"import" ~ rule_annotation? ~ ident ~ ( "::" ~ ident )* ~ ( "::" ~ "{" ~ ident_list ~ "}")?  ~ !"::" }
// rule_annotation = {"(rule)"}
fn build_ast_from_import_stmt(pair: Pair<Rule>) -> IResult<Import> {
    // initialize import ast
    let mut import_stmt = Import::place_holder();

    // Store all the possible path in the import statement
    let mut path_list: Vec<PathBuf> = vec![PathBuf::new()];
    // All the imported Rules are stored here
    // ::{a, b} expression must be single not two of them
    // e.g. not allowed ::{a, b}::{c, d}
    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::ident => {
                // At this moment, the number of file path must be 1.
                // push the file name to the first path
                path_list[0].push(inner_pair.as_str());
            }
            Rule::ident_list => {
                // This must be the end of the import definition
                let current_path = path_list[0].clone();
                // flush the current path
                path_list = vec![];
                for inner_inner_pair in inner_pair.into_inner() {
                    let mut final_path = current_path.clone();
                    final_path.push(inner_inner_pair.as_str());
                    path_list.push(final_path);
                }
            }
            Rule::rule_annotation => {
                import_stmt.rule_import();
                // parse Rule
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }

    import_stmt.set_path(path_list.clone());
    if import_stmt.rule_import {
        // if this import statement improt the rules parse it and store in the vector
        rule_importing(&mut import_stmt, path_list).unwrap()
    }
    Ok(import_stmt)
}

fn rule_importing(import_stmt: &mut Import, path_vec: Vec<PathBuf>) -> IResult<()> {
    // read path and generate RuLa AST
    for path in path_vec.iter() {
        // read the file
        let mut file_contents = String::new();
        let mut file_name = path.parent().unwrap().to_str().unwrap().to_string();
        file_name.push_str(".rula");
        println!(
            "file_name: {}, {:#?}",
            file_name,
            env::current_dir().unwrap()
        );
        match File::open(&file_name) {
            Ok(mut file_content) => {
                file_content
                    .read_to_string(&mut file_contents)
                    .expect("Failed to read the file");
            }
            Err(err) => {
                panic!("Failed to open file at {:#?}, {}", file_name, err);
            }
        };

        // get filename
        // The last file name must be the same as the rule nam
        let rule_name = path.file_name().unwrap().to_str().unwrap();
        // parse file contents
        let ast = parse(&file_contents).unwrap();
        // extract rule with rule name

        let extracted_rule = extract_rule(ast, rule_name).unwrap();
        import_stmt.add_rule_name(String::from(rule_name));
        import_stmt.add_rule(extracted_rule);
    }
    Ok(())
}

fn extract_rule(ast: AstNode, rule_name: &str) -> IResult<RuleExpr> {
    match ast {
        AstNode::RuLa(rula) => {
            match &*rula.rula {
                RuLaKind::Program(programs) => {
                    let mut target_rule = RuleExpr::place_holder();
                    for program in programs.programs.iter() {
                        match program {
                            ProgramKind::RuleExpr(rule) => {
                                // Check rule name
                                if &*rule.name.name == rule_name {
                                    target_rule = rule.clone();
                                    break;
                                }
                            }
                            _ => return Err(RuLaError::RuLaSyntaxError),
                        }
                    }
                    Ok(target_rule)
                }
                _ => return Err(RuLaError::RuLaSyntaxError),
            }
        }
        AstNode::PlaceHolder => return Err(RuLaError::RuLaSyntaxError),
    }
}

fn build_ast_from_ruleset_stmt(pair: Pair<Rule>) -> IResult<RuleSetExpr> {
    let mut ruleset_stmt = RuleSetExpr::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::ident => {
                ruleset_stmt.add_name(build_ast_from_ident(block).unwrap());
            }
            Rule::stmt => {
                ruleset_stmt
                    .add_stmt(build_ast_from_stmt(block.into_inner().next().unwrap()).unwrap());
            }
            _ => unreachable!(),
        }
    }
    Ok(ruleset_stmt)
}

fn build_ast_from_rule_stmt(pair: Pair<Rule>) -> IResult<RuleExpr> {
    let mut rule_stmt = RuleExpr::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::ident => {
                // rule_name
                rule_stmt.add_name(build_ast_from_ident(block).unwrap());
            }
            Rule::repeater_ident => {
                // TODO: Should this be vector?
                rule_stmt.add_repeater_ident(
                    build_ast_from_ident(block.into_inner().next().unwrap()).unwrap(),
                );
            }
            Rule::argument_def => {
                for arg in block.into_inner() {
                    match arg.as_rule() {
                        Rule::ident_typed => {
                            rule_stmt.add_arg(build_ast_from_ident_typed(arg).unwrap())
                        }
                        Rule::ident => rule_stmt.add_arg(build_ast_from_ident(arg).unwrap()),
                        _ => return Err(RuLaError::RuLaSyntaxError),
                    }
                }
            }
            Rule::ret_type_annotation => rule_stmt.add_return_type_annotation(Some(
                build_ast_from_return_type_annotation(block).unwrap(),
            )),
            Rule::rule_contents => {
                rule_stmt.add_rule_content(build_ast_from_rule_contents(block).unwrap())
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(rule_stmt)
}

fn build_ast_from_return_type_annotation(pair: Pair<Rule>) -> IResult<ReturnTypeAnnotation> {
    let mut ret_type_annotation = ReturnTypeAnnotation::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::typedef_lit => {
                ret_type_annotation.add_type_def(
                    build_ast_from_typedef_lit(block.into_inner().next().unwrap()).unwrap(),
                );
            }
            Rule::maybe => {
                ret_type_annotation.update_maybe();
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(ret_type_annotation)
}

fn build_ast_from_rule_contents(pair: Pair<Rule>) -> IResult<RuleContentExpr> {
    let mut rule_content_expr = RuleContentExpr::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::let_stmt => {
                rule_content_expr.add_pre_let_stmt(build_ast_from_let_stmt(block).unwrap())
            }
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

fn build_ast_from_cond_expr(pair: Pair<Rule>) -> IResult<CondExpr> {
    let mut cond_expr = CondExpr::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::res_assign => cond_expr.add_condition_clause(CondClauses::ResAssign(
                build_ast_from_res_assign(block).unwrap(),
            )),
            Rule::fn_call_expr => cond_expr.add_condition_clause(CondClauses::FnCall(
                build_ast_from_fn_call_expr(block).unwrap(),
            )),
            Rule::variable_call_expr => cond_expr.add_condition_clause(
                CondClauses::VariableCallExpr(build_ast_from_variable_call_expr(block).unwrap()),
            ),
            Rule::comp_expr => cond_expr
                .add_condition_clause(CondClauses::Comp(build_ast_from_comp_expr(block).unwrap())),
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(cond_expr)
}

fn build_ast_from_res_assign(pair: Pair<Rule>) -> IResult<ResAssign> {
    let mut res_assign = ResAssign::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::ident => {
                res_assign.add_res_name(build_ast_from_ident(block).unwrap());
            }
            Rule::fn_call_expr => {
                res_assign.add_fn_call(build_ast_from_fn_call_expr(block).unwrap());
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(res_assign)
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

// Parse If expr <--> {paren_expr | brace_stmt | else_if | else} semi endpoint
fn build_ast_from_if_stmt(pair: Pair<Rule>) -> IResult<If> {
    // `pair` structure
    // if_stmt -> inner {paren_expr, block_expr}
    let mut if_stmt = If::place_holder(); // initialize if expr
    for expr in pair.into_inner() {
        match expr.as_rule() {
            // block statement
            Rule::if_block => {
                if_stmt
                    .add_block(build_ast_from_if_block(expr.into_inner().next().unwrap()).unwrap());
            }
            Rule::stmt => {
                // nested expr (brace stmt -> stmt)
                if_stmt.add_stmt(build_ast_from_stmt(expr.into_inner().next().unwrap()).unwrap());
            }
            Rule::else_if_stmt => {
                // recursively apply
                if_stmt.add_elif(build_ast_from_if_stmt(expr).unwrap());
            }
            Rule::else_stmt => {
                let else_stmt = build_ast_from_stmt(
                    expr.into_inner()
                        .next()
                        .unwrap()
                        .into_inner()
                        .next()
                        .unwrap(),
                )
                .unwrap();
                if_stmt.add_else(else_stmt);
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(if_stmt)
}

fn build_ast_from_if_block(pair: Pair<Rule>) -> IResult<IfBlock> {
    match pair.as_rule() {
        Rule::get_expr => Ok(IfBlock::Get(build_ast_from_get_expr(pair).unwrap())),
        Rule::comp_expr => Ok(IfBlock::Comp(build_ast_from_comp_expr(pair).unwrap())),
        Rule::literal_expr => Ok(IfBlock::Lit(
            build_ast_from_literals(pair.into_inner().next().unwrap()).unwrap(),
        )),
        _ => return Err(RuLaError::RuLaSyntaxError),
    }
}

fn build_ast_from_for_stmt(pair: Pair<Rule>) -> IResult<For> {
    let mut for_stmt = For::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::ident => {
                for_stmt.add_ident(build_ast_from_ident(block).unwrap());
            }
            Rule::ident_list => {
                // omitted (patterns)
                // for identifier lists (ident, ident2, ...)
                for ident in block.into_inner() {
                    // get ident
                    for_stmt.add_ident(build_ast_from_ident(ident).unwrap());
                }
            }
            Rule::series => {
                for_stmt.add_generator(ForGenerator::Series(
                    build_ast_from_series_expr(block).unwrap(),
                ));
            }
            Rule::expr => for_stmt.add_generator(ForGenerator::Expr(
                build_ast_from_expr(block.into_inner().next().unwrap()).unwrap(),
            )),
            Rule::stmt => {
                for_stmt.add_stmt(build_ast_from_stmt(block.into_inner().next().unwrap()).unwrap());
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(for_stmt)
}

fn build_ast_from_series_expr(pair: Pair<Rule>) -> IResult<Series> {
    let mut series_expr = Series::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::int => series_expr.add_start(build_ast_from_integer_lit(block).unwrap()),
            Rule::expr => series_expr
                .add_end(build_ast_from_expr(block.into_inner().next().unwrap()).unwrap()),
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(series_expr)
}

fn build_ast_from_match_stmt(pair: Pair<Rule>) -> IResult<Match> {
    let mut match_stmt = Match::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::expr => match_stmt
                .add_expr(build_ast_from_expr(block.into_inner().next().unwrap()).unwrap()),
            Rule::match_arm => match_stmt.add_match_arm(build_ast_from_match_arm(block).unwrap()),
            // Here this can only be finally:
            Rule::match_action => {
                match_stmt.add_finally(Some(build_ast_from_match_action(block).unwrap()))
            }
            // Rule::ident => match_stmt.
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(match_stmt)
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
            Rule::stmt => match_action
                .add_actionable(build_ast_from_stmt(block.into_inner().next().unwrap()).unwrap()),
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(match_action)
}

fn build_ast_from_promote_stmt(pair: Pair<Rule>) -> IResult<Promote> {
    let mut promote_stmt = Promote::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::comp_expr => {
                promote_stmt.add_target(Promotables::Comp(
                    build_ast_from_comp_expr(block.into_inner().next().unwrap()).unwrap(),
                ));
            }
            Rule::term_expr => {
                promote_stmt.add_target(Promotables::Term(
                    build_ast_from_term_expr(block.into_inner().next().unwrap()).unwrap(),
                ));
            }
            Rule::vector => {
                promote_stmt.add_target(Promotables::RuLaVec(
                    build_ast_from_vector(block.into_inner().next().unwrap()).unwrap(),
                ));
            }
            Rule::tuple => {
                promote_stmt.add_target(Promotables::RuLaTuple(
                    build_ast_from_tuple(block.into_inner().next().unwrap()).unwrap(),
                ));
            }
            Rule::variable_call_expr => {
                promote_stmt.add_target(Promotables::VariableCall(
                    build_ast_from_variable_call_expr(block.into_inner().next().unwrap()).unwrap(),
                ));
            }
            Rule::literal_expr => {
                promote_stmt.add_target(Promotables::Lit(
                    build_ast_from_literals(block.into_inner().next().unwrap()).unwrap(),
                ));
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(promote_stmt)
}

fn build_ast_from_send_stmt(pair: Pair<Rule>) -> IResult<Send> {
    let mut send_stmt = Send::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::fn_call_expr => {
                let generated = build_ast_from_fn_call_expr(block).unwrap();
                match &*generated.func_name.name as &str {
                    "update" => { /*ok */ }
                    "free" => { /*ok */ }
                    "transfer" => { /*ok */ }
                    "meas" => { /*ok */ }
                    _ => return Err(RuLaError::RuLaSyntaxError),
                }
                send_stmt.add_fn_call(generated);
            }
            Rule::expr => {
                send_stmt.add_expr(build_ast_from_expr(block.into_inner().next().unwrap()).unwrap())
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }

    Ok(send_stmt)
}

fn build_ast_from_set_stmt(pair: Pair<Rule>) -> IResult<Set> {
    let mut set_stmt = Set::place_holder();
    let mut count = 0;
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::ident => {
                if count == 0 {
                    set_stmt.set_value(build_ast_from_ident(block).unwrap());
                    count += 1;
                } else if count == 1 {
                    set_stmt.set_alias(Some(build_ast_from_ident(block).unwrap()))
                } else {
                    panic!("Unknown error in set expression")
                }
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(set_stmt)
}

fn build_ast_from_get_expr(pair: Pair<Rule>) -> IResult<Get> {
    let mut get_expr = Get::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::ident => get_expr.set_value(build_ast_from_ident(block).unwrap()),
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(get_expr)
}

fn build_ast_from_fn_call_expr(pair: Pair<Rule>) -> IResult<FnCall> {
    let mut fnc_call = FnCall::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::ident => {
                fnc_call.add_name(build_ast_from_ident(block).unwrap());
            }
            Rule::repeater_ident => {
                fnc_call.add_name(build_ast_from_ident(block).unwrap());
                // This is repeater call
                fnc_call.update_repeater_call();
            }
            Rule::fn_call_args => {
                fnc_call.add_argument(
                    build_ast_from_fn_call_args(block.into_inner().next().unwrap()).unwrap(),
                );
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(fnc_call)
}

fn build_ast_from_fn_call_args(pair: Pair<Rule>) -> IResult<FnCallArgs> {
    match pair.as_rule() {
        Rule::fn_call_expr => Ok(FnCallArgs::FnCall(
            build_ast_from_fn_call_expr(pair).unwrap(),
        )),
        Rule::variable_call_expr => Ok(FnCallArgs::VariableCall(
            build_ast_from_variable_call_expr(pair).unwrap(),
        )),
        Rule::term_expr => Ok(FnCallArgs::Term(build_ast_from_term_expr(pair).unwrap())),
        Rule::literal_expr => Ok(FnCallArgs::Lit(
            build_ast_from_literals(pair.into_inner().next().unwrap()).unwrap(),
        )),
        _ => return Err(RuLaError::RuLaSyntaxError),
    }
}

fn build_ast_from_rule_call_expr(pair: Pair<Rule>) -> IResult<RuleCall> {
    let mut rule_call_expr = RuleCall::place_holder();
    for block in pair.into_inner() {
        // rule_call_expr.
        match block.as_rule() {
            // rule name
            Rule::ident => rule_call_expr.update_rule_name(build_ast_from_ident(block).unwrap()),
            Rule::repeater_call => rule_call_expr.add_repeater_arg(
                build_ast_from_repeater_arg(block.into_inner().next().unwrap()).unwrap(),
            ),
            Rule::fn_call_args => {
                rule_call_expr.add_arg(
                    build_ast_from_fn_call_args(block.into_inner().next().unwrap()).unwrap(),
                );
            }
            _ => todo!("here?{:#?}", block), // _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(rule_call_expr)
}

fn build_ast_from_repeater_arg(pair: Pair<Rule>) -> IResult<RepeaterCallArg> {
    match pair.as_rule() {
        Rule::term_expr => Ok(RepeaterCallArg::Term(
            build_ast_from_term_expr(pair).unwrap(),
        )),
        Rule::ident => Ok(RepeaterCallArg::Ident(build_ast_from_ident(pair).unwrap())),
        Rule::int => Ok(RepeaterCallArg::IntegerLit(
            build_ast_from_integer_lit(pair).unwrap(),
        )),
        _ => return Err(RuLaError::RuLaSyntaxError),
    }
}

fn build_ast_from_comp_expr(pair: Pair<Rule>) -> IResult<Comp> {
    let mut comp_op = CompOpKind::PlaceHolder;
    let mut comp_expr = Comp::place_holder();
    let mut expressions = vec![];
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::comparable => {
                expressions
                    .push(build_ast_from_comparable(block.into_inner().next().unwrap()).unwrap());
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
            _ => return Err(RuLaError::RuLaSyntaxError),
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

fn build_ast_from_comparable(pair: Pair<Rule>) -> IResult<Comparable> {
    match pair.as_rule() {
        Rule::get_expr => Ok(Comparable::Get(build_ast_from_get_expr(pair).unwrap())),
        Rule::term_expr => Ok(Comparable::Term(build_ast_from_term_expr(pair).unwrap())),
        Rule::variable_call_expr => Ok(Comparable::VariableCall(
            build_ast_from_variable_call_expr(pair).unwrap(),
        )),
        Rule::fn_call_expr => Ok(Comparable::FnCall(
            build_ast_from_fn_call_expr(pair).unwrap(),
        )),
        Rule::literal_expr => Ok(Comparable::Lit(
            build_ast_from_literals(pair.into_inner().next().unwrap()).unwrap(),
        )),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

fn build_ast_from_term_expr(pair: Pair<Rule>) -> IResult<Term> {
    let mut term_expr = Term::place_holder();
    let mut is_left = true;
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::terms => {
                if is_left {
                    term_expr
                        .add_lhs(build_ast_from_terms(block.into_inner().next().unwrap()).unwrap());
                    is_left = false;
                } else {
                    term_expr
                        .add_rhs(build_ast_from_terms(block.into_inner().next().unwrap()).unwrap());
                }
            }
            Rule::term_expr => {
                if is_left {
                    term_expr.add_lhs(Terms::Term(build_ast_from_term_expr(block).unwrap()));
                    is_left = false;
                } else {
                    term_expr.add_rhs(Terms::Term(build_ast_from_term_expr(block).unwrap()));
                }
            }
            Rule::op => match block.as_str() {
                "+" => {
                    term_expr.add_op(TermOps::Plus);
                }
                "-" => {
                    term_expr.add_op(TermOps::Minus);
                }
                "*" => {
                    term_expr.add_op(TermOps::Asterisk);
                }
                "/" => {
                    term_expr.add_op(TermOps::Slash);
                }
                "%" => {
                    term_expr.add_op(TermOps::Percent);
                }
                "^" => {
                    term_expr.add_op(TermOps::Caret);
                }
                _ => return Err(RuLaError::RuLaSyntaxError),
            },
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(term_expr)
}

fn build_ast_from_terms(pair: Pair<Rule>) -> IResult<Terms> {
    match pair.as_rule() {
        Rule::variable_call_expr => Ok(Terms::VariableCallExpr(
            build_ast_from_variable_call_expr(pair).unwrap(),
        )),
        Rule::fn_call_expr => Ok(Terms::FnCall(build_ast_from_fn_call_expr(pair).unwrap())),
        Rule::literal_expr => Ok(Terms::Lit(
            build_ast_from_literals(pair.into_inner().next().unwrap()).unwrap(),
        )),
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
        Rule::repeater_ident => Ok(Callable::RepeaterIdent(
            build_ast_from_ident(pair.into_inner().next().unwrap()).unwrap(),
        )),
        Rule::fn_call_expr => Ok(Callable::FnCall(build_ast_from_fn_call_expr(pair).unwrap())),
        _ => Err(RuLaError::RuLaSyntaxError),
    }
}

fn build_ast_from_vector(pair: Pair<Rule>) -> IResult<RuLaVec> {
    let mut rula_vec = RuLaVec::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::literal_expr => {
                rula_vec
                    .add_item(build_ast_from_literals(block.into_inner().next().unwrap()).unwrap());
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(rula_vec)
}

fn build_ast_from_tuple(pair: Pair<Rule>) -> IResult<RuLaTuple> {
    let mut rula_tuple = RuLaTuple::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::expr => {
                rula_tuple
                    .add_item(build_ast_from_expr(block.into_inner().next().unwrap()).unwrap());
            }
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    Ok(rula_tuple)
}

// Parse Literals <--> {string literal | boolean literal}
fn build_ast_from_literals(pair: Pair<Rule>) -> IResult<Lit> {
    match pair.as_rule() {
        // identifier
        Rule::ident => Ok(Lit::new(LitKind::Ident(
            build_ast_from_ident(pair).unwrap(),
        ))),
        Rule::raw_string => Ok(Lit::new(LitKind::StringLit(StringLit::new(pair.as_str())))),
        Rule::number => {
            let num_lit = build_ast_from_num_lit(pair).unwrap();
            Ok(Lit::new(LitKind::NumberLit(num_lit)))
        }
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

fn build_ast_from_num_lit(pair: Pair<Rule>) -> IResult<NumberLit> {
    let mut negative = false;
    let mut number_lit = NumberLit::place_holder();
    for block in pair.into_inner() {
        match block.as_rule() {
            Rule::minus => negative = true,
            Rule::int => {
                number_lit.update_value(NumberLitKind::IntegerLit(
                    build_ast_from_integer_lit(block).unwrap(),
                ));
            }
            Rule::float => number_lit.update_value(NumberLitKind::FloatLit(
                build_ast_from_float_lit(block).unwrap(),
            )),
            Rule::ident => number_lit.update_value(NumberLitKind::NumIdentLit(NumIdentLit::new(
                build_ast_from_ident(block).unwrap(),
                false,
            ))),
            _ => return Err(RuLaError::RuLaSyntaxError),
        }
    }
    // This operation is oder sensitive.
    // The value must be set before the negative is activated
    if negative {
        number_lit.negative()
    }
    Ok(number_lit)
}

fn build_ast_from_integer_lit(pair: Pair<Rule>) -> IResult<IntegerLit> {
    let int_lit = IntegerLit::new(pair.as_str(), false);
    Ok(int_lit)
}

fn build_ast_from_float_lit(pair: Pair<Rule>) -> IResult<FloatLit> {
    let float_lit = FloatLit::new(pair.as_str(), false);
    Ok(float_lit)
}

fn build_ast_from_typedef_lit(pair: Pair<Rule>) -> IResult<TypeDef> {
    match pair.as_rule() {
        Rule::integer_type => match pair.as_str() {
            "int" => return Ok(TypeDef::Integer),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        Rule::unsigned_integer_type => match pair.as_str() {
            "u_int" => return Ok(TypeDef::UnsignedInteger),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        Rule::float_type => match pair.as_str() {
            "float" => return Ok(TypeDef::Float),
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
        Rule::repeater_type => match pair.as_str() {
            "Repeater" => return Ok(TypeDef::Repeater),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        Rule::message_type => match pair.as_str() {
            "Message" => return Ok(TypeDef::Message),
            _ => return Err(RuLaError::RuLaSyntaxError),
        },
        Rule::result_type => match pair.as_str() {
            "Result" => return Ok(TypeDef::Result),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pest::Parser;
    use crate::RuLaParser;

    fn pair_generator(source: &str, rule: Rule) -> Pair<Rule> {
        let pairs = RuLaParser::parse(rule, source).unwrap();
        pairs.into_iter().next().unwrap()
    }

    #[cfg(test)]
    mod let_stmt_tests {
        use super::*;

        #[test]
        fn test_simple_let_stmt() {
            let let_stmt = pair_generator(r#"let hello: str = "world""#, Rule::let_stmt);
            let let_ast_nodes = build_ast_from_let_stmt(let_stmt).unwrap();
            let target_ast_nodes = Let::new(
                vec![Ident::new("hello", Some(TypeDef::Str))],
                Expr::new(ExprKind::Lit(Lit::new(LitKind::StringLit(StringLit::new(
                    "world",
                ))))),
            );
            assert_eq!(let_ast_nodes, target_ast_nodes);
        }

        #[test]
        fn test_simple_let_stmt_int() {
            let let_stmt = pair_generator(r#"let hello:int = -123"#, Rule::let_stmt);
            let let_ast_nodes = build_ast_from_let_stmt(let_stmt).unwrap();
            let target_ast_nodes = Let::new(
                vec![Ident::new("hello", Some(TypeDef::Integer))],
                Expr::new(ExprKind::Lit(Lit::new(LitKind::NumberLit(NumberLit::new(
                    NumberLitKind::IntegerLit(IntegerLit::new("123", true)),
                ))))),
            );
            assert_eq!(let_ast_nodes, target_ast_nodes);
        }
    }

    #[cfg(test)]
    mod if_stmt_tests {
        use super::*;

        #[test]
        fn test_single_if_stmt() {
            let if_stmt = pair_generator("if(block){expression}", Rule::if_stmt);
            let if_ast_nodes = build_ast_from_if_stmt(if_stmt).unwrap();
            let target_ast_nodes = If::new(
                // (block)
                IfBlock::Lit(Lit::new(LitKind::Ident(Ident::new("block", None)))),
                // {expression}
                vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
                    Lit::new(LitKind::Ident(Ident::new("expression", None))),
                ))))],
                // elif ~
                vec![],
                // else ~
                vec![],
            );
            assert_eq!(target_ast_nodes, if_ast_nodes);
        }

        #[test]
        fn test_if_else_stmt() {
            let if_else = pair_generator("if(block){expression}else{expression2}", Rule::if_stmt);
            let if_else_ast_nodes = build_ast_from_if_stmt(if_else).unwrap();
            let target_ast_nodes = If::new(
                // (block)
                IfBlock::Lit(Lit::new(LitKind::Ident(Ident::new("block", None)))),
                // {expression}
                vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
                    Lit::new(LitKind::Ident(Ident::new("expression", None))),
                ))))],
                // elif ~
                vec![],
                // else ~
                vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
                    Lit::new(LitKind::Ident(Ident::new("expression2", None))),
                ))))],
            );
            assert_eq!(target_ast_nodes, if_else_ast_nodes);
        }

        #[test]
        fn test_if_elif_stmt() {
            let if_elif_stmt = pair_generator(
                "if(block){expression} else if (block2){expression2}",
                Rule::if_stmt,
            );
            let if_elif_ast_nodes = build_ast_from_if_stmt(if_elif_stmt).unwrap();
            let target_ast_nodes = If::new(
                // (block)
                IfBlock::Lit(Lit::new(LitKind::Ident(Ident::new("block", None)))),
                // {expression}
                vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
                    Lit::new(LitKind::Ident(Ident::new("expression", None))),
                ))))],
                // elif ~
                vec![If::new(
                    // else if (block)
                    IfBlock::Lit(Lit::new(LitKind::Ident(Ident::new("block2", None)))),
                    // else if () {statement2;};
                    vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
                        Lit::new(LitKind::Ident(Ident::new("expression2", None))),
                    ))))],
                    vec![],
                    vec![],
                )],
                // else ~
                vec![],
            );
            assert_eq!(target_ast_nodes, if_elif_ast_nodes);
        }

        #[test]
        fn test_if_elif_else_stmt() {
            let if_elif_stmt = pair_generator(
                "if(block){expression} else if (block2){expression2} else {expression3}",
                Rule::if_stmt,
            );
            let if_elif_ast_nodes = build_ast_from_if_stmt(if_elif_stmt).unwrap();
            let target_ast_nodes = If::new(
                // (block)
                IfBlock::Lit(Lit::new(LitKind::Ident(Ident::new("block", None)))),
                // {expression}
                vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
                    Lit::new(LitKind::Ident(Ident::new("expression", None))),
                ))))],
                // elif ~
                vec![If::new(
                    // else if (block)
                    IfBlock::Lit(Lit::new(LitKind::Ident(Ident::new("block2", None)))),
                    // else if () {statement2;};
                    vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
                        Lit::new(LitKind::Ident(Ident::new("expression2", None))),
                    ))))],
                    vec![],
                    vec![],
                )],
                // else ~
                vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
                    Lit::new(LitKind::Ident(Ident::new("expression3", None))),
                ))))],
            );
            assert_eq!(target_ast_nodes, if_elif_ast_nodes);
        }
        // Add error test here
    }

    #[cfg(test)]
    mod for_stmt_test {
        use super::*;

        #[test]
        fn test_simple_for_stmt() {
            // divition is tricky a little
            let for_stmt = pair_generator("for i in range(){hello}", Rule::for_stmt);
            let for_asts = build_ast_from_for_stmt(for_stmt).unwrap();
            let target_ast_nodes = For::new(
                vec![Ident::new("i", None)],
                ForGenerator::Expr(Expr::new(ExprKind::FnCall(FnCall::new(
                    Ident::new("range", None),
                    false,
                    vec![],
                )))),
                vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
                    Lit::new(LitKind::Ident(Ident::new("hello", None))),
                ))))],
            );
            assert_eq!(target_ast_nodes, for_asts);
        }

        #[test]
        fn test_multi_arg_for_stmt() {
            // divition is tricky a little
            let for_stmt = pair_generator("for (a, b, c) in generator{hello}", Rule::for_stmt);
            let for_asts = build_ast_from_for_stmt(for_stmt).unwrap();
            let target_ast_nodes = For::new(
                vec![
                    Ident::new("a", None),
                    Ident::new("b", None),
                    Ident::new("c", None),
                ],
                ForGenerator::Expr(Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(
                    Ident::new("generator", None),
                ))))),
                vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
                    Lit::new(LitKind::Ident(Ident::new("hello", None))),
                ))))],
            );
            assert_eq!(target_ast_nodes, for_asts);
        }
    }

    #[cfg(test)]
    mod test_match_stmt {
        use super::*;

        #[test]
        fn test_simple_match_no_otherwise() {
            let match_stmt = pair_generator(
                "match test{
            true => {something()},
            false => {otherthing()},
        }",
                Rule::match_stmt,
            );
            let match_asts = build_ast_from_match_stmt(match_stmt).unwrap();
            let target_ast_nodes = Match::new(
                Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                    "test", None,
                ))))),
                vec![
                    MatchArm::new(
                        MatchCondition::new(Satisfiable::Lit(Lit::new(LitKind::BooleanLit(true)))),
                        MatchAction::new(vec![Stmt::new(StmtKind::Expr(Expr::new(
                            ExprKind::FnCall(FnCall::new(
                                Ident::new("something", None),
                                false,
                                vec![],
                            )),
                        )))]),
                    ),
                    MatchArm::new(
                        MatchCondition::new(Satisfiable::Lit(Lit::new(LitKind::BooleanLit(false)))),
                        MatchAction::new(vec![Stmt::new(StmtKind::Expr(Expr::new(
                            ExprKind::FnCall(FnCall::new(
                                Ident::new("otherthing", None),
                                false,
                                vec![],
                            )),
                        )))]),
                    ),
                ],
                None,
            );
            assert_eq!(target_ast_nodes, match_asts);
        }

        #[test]
        fn test_simple_match_with_otherwise() {
            let match_stmt = pair_generator(
                "match test{
            true => {something()},
            false => {otherthing()},
            otherwise => {final()}
        }",
                Rule::match_stmt,
            );
            let match_asts = build_ast_from_match_stmt(match_stmt).unwrap();
            let target_ast_nodes = Match::new(
                Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                    "test", None,
                ))))),
                vec![
                    MatchArm::new(
                        MatchCondition::new(Satisfiable::Lit(Lit::new(LitKind::BooleanLit(true)))),
                        MatchAction::new(vec![Stmt::new(StmtKind::Expr(Expr::new(
                            ExprKind::FnCall(FnCall::new(
                                Ident::new("something", None),
                                false,
                                vec![],
                            )),
                        )))]),
                    ),
                    MatchArm::new(
                        MatchCondition::new(Satisfiable::Lit(Lit::new(LitKind::BooleanLit(false)))),
                        MatchAction::new(vec![Stmt::new(StmtKind::Expr(Expr::new(
                            ExprKind::FnCall(FnCall::new(
                                Ident::new("otherthing", None),
                                false,
                                vec![],
                            )),
                        )))]),
                    ),
                ],
                Some(MatchAction::new(vec![Stmt::new(StmtKind::Expr(
                    Expr::new(ExprKind::FnCall(FnCall::new(
                        Ident::new("final", None),
                        false,
                        vec![],
                    ))),
                ))])),
            );
            assert_eq!(target_ast_nodes, match_asts);
        }
    }

    #[cfg(test)]
    mod test_return_expr {
        use super::*;

        #[test]
        fn test_simple_promote_stmt() {
            let promote_stmt = pair_generator("promote hello", Rule::promote_stmt);
            let return_asts = build_ast_from_promote_stmt(promote_stmt).unwrap();
            let target_ast_nodes = Promote::new(vec![Promotables::Lit(Lit::new(LitKind::Ident(
                Ident::new("hello", None),
            )))]);
            assert_eq!(target_ast_nodes, return_asts);
        }
    }

    #[cfg(test)]
    mod test_literals {
        use super::*;

        #[test]
        fn test_boolean_true_literal() {
            // divition is tricky a little
            let lit_expr = pair_generator("true", Rule::literal_expr);
            let lit_asts = build_ast_from_literals(lit_expr.into_inner().next().unwrap()).unwrap();
            let target_ast_nodes = Lit::new(LitKind::BooleanLit(true));
            assert_eq!(target_ast_nodes, lit_asts);
        }

        #[test]
        fn test_boolean_false_literal() {
            // divition is tricky a little
            let lit_expr = pair_generator("false", Rule::literal_expr);
            let lit_asts = build_ast_from_literals(lit_expr.into_inner().next().unwrap()).unwrap();
            let target_ast_nodes = Lit::new(LitKind::BooleanLit(false));
            assert_eq!(target_ast_nodes, lit_asts);
        }

        // helper function
        fn generate_type_lit_ast(name: &str, type_def: Option<TypeDef>) -> Let {
            let target_ast_nodes = Let::new(
                vec![Ident::new(name, type_def)],
                Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
                    "val", None,
                ))))),
            );
            target_ast_nodes
        }

        #[test]
        fn test_type_literals() {
            // divition is tricky a little
            let lit_expr = pair_generator("let number:int = val", Rule::let_stmt);
            let lit_asts = build_ast_from_let_stmt(lit_expr).unwrap();
            let target_ast_nodes = generate_type_lit_ast("number", Some(TypeDef::Integer));
            assert_eq!(target_ast_nodes, lit_asts);

            let lit_expr = pair_generator("let number:float = val", Rule::let_stmt);
            let lit_asts = build_ast_from_let_stmt(lit_expr).unwrap();
            let target_ast_nodes = generate_type_lit_ast("number", Some(TypeDef::Float));
            assert_eq!(target_ast_nodes, lit_asts);

            let lit_expr = pair_generator("let number:u_int = val", Rule::let_stmt);
            let lit_asts = build_ast_from_let_stmt(lit_expr).unwrap();
            let target_ast_nodes = generate_type_lit_ast("number", Some(TypeDef::UnsignedInteger));
            assert_eq!(target_ast_nodes, lit_asts);

            let lit_expr = pair_generator("let number:bool = val", Rule::let_stmt);
            let lit_asts = build_ast_from_let_stmt(lit_expr).unwrap();
            let target_ast_nodes = generate_type_lit_ast("number", Some(TypeDef::Boolean));
            assert_eq!(target_ast_nodes, lit_asts);

            let lit_expr = pair_generator("let number:str = val", Rule::let_stmt);
            let lit_asts = build_ast_from_let_stmt(lit_expr).unwrap();
            let target_ast_nodes = generate_type_lit_ast("number", Some(TypeDef::Str));
            assert_eq!(target_ast_nodes, lit_asts);

            let lit_expr = pair_generator("let number:Qubit = val", Rule::let_stmt);
            let lit_asts = build_ast_from_let_stmt(lit_expr).unwrap();
            let target_ast_nodes = generate_type_lit_ast("number", Some(TypeDef::Qubit));
            assert_eq!(target_ast_nodes, lit_asts);
        }

        #[test]
        fn test_binary_literals() {
            let lit_expr = pair_generator("0b0100100", Rule::literal_expr);
            let lit_asts = build_ast_from_literals(lit_expr.into_inner().next().unwrap()).unwrap();
            let target_ast_nodes = Lit::new(LitKind::BinaryLit(BinaryLit::new("0100100")));
            assert_eq!(target_ast_nodes, lit_asts);
        }

        #[test]
        fn test_hex_literals() {
            let lit_expr = pair_generator("0x0e8afc", Rule::literal_expr);
            let lit_asts = build_ast_from_literals(lit_expr.into_inner().next().unwrap()).unwrap();
            let target_ast_nodes = Lit::new(LitKind::HexLit(HexLit::new("0e8afc")));
            assert_eq!(target_ast_nodes, lit_asts);
        }

        #[test]
        fn test_unicord_literals() {
            let lit_expr = pair_generator("0u1F680", Rule::literal_expr); //🚀
            let lit_asts = build_ast_from_literals(lit_expr.into_inner().next().unwrap()).unwrap();
            let target_ast_nodes = Lit::new(LitKind::UnicordLit(UnicordLit::new("1F680")));
            assert_eq!(target_ast_nodes, lit_asts);
        }
    }

    #[cfg(test)]
    mod test_variable_call {
        use super::*;

        #[test]
        fn test_simple_variable_call() {
            let var_call = pair_generator("test.hello", Rule::variable_call_expr);
            let var_call_ast = build_ast_from_variable_call_expr(var_call).unwrap();
            let target_ast_nodes = VariableCallExpr::new(vec![
                Callable::Ident(Ident::new("test", None)),
                Callable::Ident(Ident::new("hello", None)),
            ]);
            assert_eq!(target_ast_nodes, var_call_ast);
        }

        #[test]
        fn test_simple_variable_fnc_call() {
            let var_call = pair_generator("test.hello()", Rule::variable_call_expr);
            let var_call_ast = build_ast_from_variable_call_expr(var_call).unwrap();
            let target_ast_nodes = VariableCallExpr::new(vec![
                Callable::Ident(Ident::new("test", None)),
                Callable::FnCall(FnCall::new(Ident::new("hello", None), false, vec![])),
            ]);
            assert_eq!(target_ast_nodes, var_call_ast);
        }
    }

    #[cfg(test)]
    mod test_rule_call_expr {
        use super::*;
        #[test]
        fn test_simple_rule_call() {
            let rule_call = pair_generator("rulename<#repeaters(0)>()", Rule::rule_call_expr);
            let rule_call_ast = build_ast_from_rule_call_expr(rule_call).unwrap();
            let target_ast_nodes = RuleCall::new(
                // rule_name
                Ident::new("rulename", None),
                RepeaterCallArg::IntegerLit(IntegerLit::new("0", false)),
                vec![],
            );
            assert_eq!(rule_call_ast, target_ast_nodes);
        }
        #[test]
        fn test_rule_call_with_args() {
            let rule_call = pair_generator(
                "rulename<#repeaters(0)>(i, a.k(), k())",
                Rule::rule_call_expr,
            );
            let rule_call_ast = build_ast_from_rule_call_expr(rule_call).unwrap();
            let target_ast_nodes = RuleCall::new(
                Ident::new("rulename", None),
                RepeaterCallArg::IntegerLit(IntegerLit::new("0", false)),
                vec![
                    FnCallArgs::Lit(Lit::new(LitKind::Ident(Ident::new("i", None)))),
                    FnCallArgs::VariableCall(VariableCallExpr::new(vec![
                        Callable::Ident(Ident::new("a", None)),
                        Callable::FnCall(FnCall::new(Ident::new("k", None), false, vec![])),
                    ])),
                    FnCallArgs::FnCall(FnCall::new(Ident::new("k", None), false, vec![])),
                ],
            );
            assert_eq!(rule_call_ast, target_ast_nodes);
        }
    }

    #[cfg(test)]
    mod test_term_expr {
        use super::*;

        #[test]
        fn test_simple_term_expr() {
            let term_expr = pair_generator("i+1", Rule::term_expr);
            let term_ast = build_ast_from_term_expr(term_expr).unwrap();
            let target_ast = Term::new(
                Terms::Lit(Lit::new(LitKind::Ident(Ident::new("i", None)))),
                TermOps::Plus,
                Terms::Lit(Lit::new(LitKind::NumberLit(NumberLit::new(
                    NumberLitKind::IntegerLit(IntegerLit::new("1", false)),
                )))),
            );
            assert_eq!(term_ast, target_ast);
        }

        #[test]
        fn test_multi_term_expr() {
            let term_expr = pair_generator("(#repeaters.len()/2) + 1", Rule::term_expr);
            let term_ast = build_ast_from_term_expr(term_expr).unwrap();
            let target_ast = Term::new(
                Terms::Term(Term::new(
                    Terms::VariableCallExpr(VariableCallExpr::new(vec![
                        Callable::RepeaterIdent(Ident::new("repeaters", None)),
                        Callable::FnCall(FnCall::new(Ident::new("len", None), false, vec![])),
                    ])),
                    TermOps::Slash,
                    Terms::Lit(Lit::new(LitKind::NumberLit(NumberLit::new(
                        NumberLitKind::IntegerLit(IntegerLit::new("2", false)),
                    )))),
                )),
                TermOps::Plus,
                Terms::Lit(Lit::new(LitKind::NumberLit(NumberLit::new(
                    NumberLitKind::IntegerLit(IntegerLit::new("1", false)),
                )))),
            );
            assert_eq!(term_ast, target_ast);
        }
    }

    #[cfg(test)]
    mod test_series_expr {
        use super::*;

        #[test]
        fn test_simple_series_expr() {
            let series_expr = pair_generator("0..10", Rule::series);
            let series_ast = build_ast_from_series_expr(series_expr).unwrap();
            let target_ast = Series::new(
                IntegerLit::new("0", false),
                Expr::new(ExprKind::Lit(Lit::new(LitKind::NumberLit(NumberLit::new(
                    NumberLitKind::IntegerLit(IntegerLit::new("10", false)),
                ))))),
            );
            assert_eq!(series_ast, target_ast);
        }

        #[test]
        fn test_series_with_fn_call() {
            let series_expr = pair_generator("0..fin()", Rule::series);
            let series_ast = build_ast_from_series_expr(series_expr).unwrap();
            let target_ast = Series::new(
                IntegerLit::new("0", false),
                Expr::new(ExprKind::FnCall(FnCall::new(
                    Ident::new("fin", None),
                    false,
                    vec![],
                ))),
            );
            assert_eq!(series_ast, target_ast);
        }

        #[test]
        fn test_series_with_term_fn_call() {
            let series_expr = pair_generator("0..fin()-1 ", Rule::series);
            let series_ast = build_ast_from_series_expr(series_expr).unwrap();
            let target_ast = Series::new(
                IntegerLit::new("0", false),
                Expr::new(ExprKind::Term(Term::new(
                    Terms::FnCall(FnCall::new(Ident::new("fin", None), false, vec![])),
                    TermOps::Minus,
                    Terms::Lit(Lit::new(LitKind::NumberLit(NumberLit::new(
                        NumberLitKind::IntegerLit(IntegerLit::new("1", false)),
                    )))),
                ))),
            );
            assert_eq!(series_ast, target_ast);
        }
    }

    #[cfg(test)]
    mod test_vector_expr {
        use super::*;

        #[test]
        fn test_simple_int_vector() {
            let vec_expr = pair_generator("[0, 1, 2, 3]", Rule::vector);
            let vec_ast = build_ast_from_vector(vec_expr).unwrap();
            let target_ast = RuLaVec::new(vec![
                Lit::new(LitKind::NumberLit(NumberLit::new(
                    NumberLitKind::IntegerLit(IntegerLit::new("0", false)),
                ))),
                Lit::new(LitKind::NumberLit(NumberLit::new(
                    NumberLitKind::IntegerLit(IntegerLit::new("1", false)),
                ))),
                Lit::new(LitKind::NumberLit(NumberLit::new(
                    NumberLitKind::IntegerLit(IntegerLit::new("2", false)),
                ))),
                Lit::new(LitKind::NumberLit(NumberLit::new(
                    NumberLitKind::IntegerLit(IntegerLit::new("3", false)),
                ))),
            ]);
            assert_eq!(vec_ast, target_ast);
        }
    }

    #[cfg(test)]
    mod test_tuple_expr {
        use super::*;

        #[test]
        fn test_simple_tuple() {
            let tuple_expr = pair_generator("(test(), fnc())", Rule::tuple);
            let tuple_ast = build_ast_from_tuple(tuple_expr).unwrap();

            let target_ast = RuLaTuple::new(vec![
                Expr::new(ExprKind::FnCall(FnCall::new(
                    Ident::new("test", None),
                    false,
                    vec![],
                ))),
                Expr::new(ExprKind::FnCall(FnCall::new(
                    Ident::new("fnc", None),
                    false,
                    vec![],
                ))),
            ]);
            assert_eq!(tuple_ast, target_ast);
        }
    }

    #[cfg(test)]
    mod test_comp_expr {
        use super::*;

        #[test]
        fn test_simple_comp() {
            let comp_expr = pair_generator("test > 0", Rule::comp_expr);
            let comp_ast = build_ast_from_comp_expr(comp_expr).unwrap();

            let target_ast = Comp::new(
                Comparable::Lit(Lit::new(LitKind::Ident(Ident::new("test", None)))),
                CompOpKind::Gt,
                Comparable::Lit(Lit::new(LitKind::NumberLit(NumberLit::new(
                    NumberLitKind::IntegerLit(IntegerLit::new("0", false)),
                )))),
            );
            assert_eq!(comp_ast, target_ast);
        }

        #[test]
        fn test_comp_with_term() {
            let comp_expr = pair_generator("test + 1 != 0", Rule::comp_expr);
            let comp_ast = build_ast_from_comp_expr(comp_expr).unwrap();

            let target_ast = Comp::new(
                Comparable::Term(Term::new(
                    Terms::Lit(Lit::new(LitKind::Ident(Ident::new("test", None)))),
                    TermOps::Plus,
                    Terms::Lit(Lit::new(LitKind::NumberLit(NumberLit::new(
                        NumberLitKind::IntegerLit(IntegerLit::new("1", false)),
                    )))),
                )),
                CompOpKind::Nq,
                Comparable::Lit(Lit::new(LitKind::NumberLit(NumberLit::new(
                    NumberLitKind::IntegerLit(IntegerLit::new("0", false)),
                )))),
            );
            assert_eq!(comp_ast, target_ast);
        }
    }
}
