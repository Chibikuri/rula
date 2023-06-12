use super::scope::Scope;
use super::{symbol_table::SymbolTable, IResult};
use rula_parser::parser::ast::*;
use ruleset_ir::instructions::*;
use ruleset_ir::RuleSetIR;
use std::cell::RefCell;
use std::rc::Rc;

// Create symbol table to track new symbols
// type RefSymbolTable = RefCell<SymbolTable>;
type RcRefCellScope = Rc<RefCell<Scope>>;

// Convert RuLa expressions into RuleSet IR
pub fn generate_ir(ast_tree: &AstNode) -> IResult<RuleSetIR> {
    // create symbol table to track new symbols
    // let symbol_table = RefCell::new(SymbolTable::new());
    // Prepare a root global scope
    let global_scope = Rc::new(RefCell::new(Scope::new("__@global")));
    global_scope.borrow_mut().set_root();

    // Generate ir segment
    let mut ir_segment = RuleSetIR::new();
    let mut generated_ir = match ast_tree {
        AstNode::RuLa(rula) => generate_rula_ir(rula, &global_scope).unwrap(),
        AstNode::PlaceHolder => todo!(),
    };

    ir_segment.merge(&mut generated_ir);
    Ok(ir_segment)
}

fn generate_rula_ir(rula: &RuLa, parent_scope: &RcRefCellScope) -> IResult<RuleSetIR> {
    // At this level, new scope is not introduced.
    // Ir segment for rula_ir
    let mut rula_ir_segment = RuleSetIR::new();
    let mut generated_ir = match &*rula.rula {
        RuLaKind::Program(program) => generate_program_ir(program, &parent_scope).unwrap(),
        RuLaKind::Ignore => {
            let mut ignorant_ir = RuleSetIR::new();
            ignorant_ir.add_instruction(Box::new(NOP::new(None)));
            ignorant_ir
        }
        _ => todo!(),
    };
    rula_ir_segment.merge(&mut generated_ir);
    Ok(rula_ir_segment)
}

fn generate_program_ir(program: &Program, parent_scope: &RcRefCellScope) -> IResult<RuleSetIR> {
    // For program level ir, prepare segment ir for all the program asts
    let mut program_ir_segment = RuleSetIR::new();
    for prog in program.programs.iter() {
        let mut generated_ir = match prog {
            ProgramKind::Import(import) => generate_import_ir(import, parent_scope).unwrap(),
            ProgramKind::RuleSetExpr(ruleset) => {
                generate_ruleset_expr_ir(ruleset, parent_scope).unwrap()
            }
            ProgramKind::RuleExpr(rule) => generate_rule_expr_ir(rule, parent_scope).unwrap(),
            ProgramKind::Repeaters => {
                todo!()
            }
        };

        program_ir_segment.merge(&mut generated_ir)
    }
    Ok(program_ir_segment)
}

fn generate_import_ir(import: &Import, parent_scope: &RcRefCellScope) -> IResult<RuleSetIR> {


    Ok(RuleSetIR::new())
}

fn generate_ruleset_expr_ir(
    ruleset_expr: &RuleSetExpr,
    parent_scope: &RcRefCellScope,
) -> IResult<RuleSetIR> {
    let ruleset_name = &*ruleset_expr.name.name;
    
    // Introduce a new scope for ruleset clause
    let ruleset_scope_name = String::from("__@ruleset_") + ruleset_name;
    let ruleset_scope = Rc::new(RefCell::new(Scope::new(&ruleset_scope_name)));
    ruleset_scope.borrow_mut().set_parent(Rc::downgrade(&parent_scope));

    // 
    Ok(RuleSetIR::new())
}

fn generate_rule_expr_ir(
    rule_expr: &RuleExpr,
    parent_scope: &RcRefCellScope,
) -> IResult<RuleSetIR> {
    let rule_name = &*rule_expr.name.name;

    // Introduce a new scope for rule clause
    let rule_scope_name = String::from("__@rule_") + rule_name;
    let rule_scope = Rc::new(RefCell::new(Scope::new(&rule_scope_name)));
    rule_scope.borrow_mut().set_parent(Rc::downgrade(&parent_scope));


    Ok(RuleSetIR::new())
}
