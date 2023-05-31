use super::symbol_table::Scope;
use super::{symbol_table::SymbolTable, IResult};
use rula_parser::parser::ast::*;
use ruleset_ir::instructions::*;
use ruleset_ir::RuleSetIR;
use std::cell::RefCell;
use std::rc::Rc;

// Create symbol table to track new symbols
type RefSymbolTable = RefCell<SymbolTable>;

// Convert RuLa expressions into RuleSet IR
pub fn generate_ir(ast_tree: &AstNode) -> IResult<RuleSetIR> {
    // create symbol table to track new symbols
    let symbol_table = RefCell::new(SymbolTable::new());
    // Prepare a root global scope
    let global_scope = Rc::new(RefCell::new(Scope::new("__@global")));
    global_scope.borrow_mut().root = true;

    // Generate ir segment
    let mut ir_segment = RuleSetIR::new();
    let mut generated_ir = match ast_tree {
        AstNode::RuLa(rula) => generate_rula_ir(rula, &symbol_table).unwrap(),
        AstNode::PlaceHolder => todo!(),
    };

    ir_segment.merge(&mut generated_ir);
    Ok(ir_segment)
}

fn generate_rula_ir(rula: &RuLa, symbol_table: &RefSymbolTable) -> IResult<RuleSetIR> {
    // In rula ast level, there is no scope introduction. It's still global scope
    // Ir segment for rula_ir
    let mut rula_ir_segment = RuleSetIR::new();
    let mut generated_ir = match &*rula.rula {
        RuLaKind::Program(program) => generate_program_ir(program, symbol_table).unwrap(),
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

fn generate_program_ir(program: &Program, symbol_table: &RefSymbolTable) -> IResult<RuleSetIR> {
    // For program level ir, prepare segment ir for all the program asts
    let mut program_ir_segment = RuleSetIR::new();
    for prog in program.programs.iter() {
        let mut generated_ir = match prog {
            ProgramKind::Import(import) => generate_import_ir(import, symbol_table).unwrap(),
            ProgramKind::RuleSetExpr(ruleset) => {
                generate_ruleset_expr_ir(ruleset, symbol_table).unwrap()
            }
            ProgramKind::RuleExpr(rule) => generate_rule_expr_ir(rule, symbol_table).unwrap(),
            ProgramKind::Repeaters => {
                todo!()
            }
        };

        program_ir_segment.merge(&mut generated_ir)
    }
    Ok(RuleSetIR::new())
}

fn generate_import_ir(import: &Import, symbol_table: &RefSymbolTable) -> IResult<RuleSetIR> {
    Ok(RuleSetIR::new())
}

fn generate_ruleset_expr_ir(
    ruleset_expr: &RuleSetExpr,
    symbol_table: &RefSymbolTable,
) -> IResult<RuleSetIR> {
    Ok(RuleSetIR::new())
}

fn generate_rule_expr_ir(
    rule_expr: &RuleExpr,
    symbol_table: &RefSymbolTable,
) -> IResult<RuleSetIR> {
    Ok(RuleSetIR::new())
}
