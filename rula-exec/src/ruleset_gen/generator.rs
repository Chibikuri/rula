use super::symbol_table::Scope;
use super::{symbol_table::SymbolTable, IResult};
use rula_parser::parser::ast::*;
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

    let generated_ir = match ast_tree {
        AstNode::RuLa(rula) => generate_rula_ir(rula, &symbol_table).unwrap(),
        AstNode::PlaceHolder => todo!(),
    };

    Ok(generated_ir)
}

pub fn generate_rula_ir(rula: &RuLa, symbol_table: &RefSymbolTable) -> IResult<RuleSetIR> {
    Ok(RuleSetIR {})
}
