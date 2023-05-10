use std::collections::HashMap;

use super::InternalResult;
use super::{error::InternalError, types::Types};
use std::cell::RefCell;
use std::rc::{Rc, Weak};

type SymbolName = String;

#[derive(Debug)]
pub struct SymbolTable {
    // scope hierarchy that can be climbed up to global scope
    pub scope_tree: Box<Scope>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            // default scope name "__@" will be replaced by global scope "__@global"
            scope_tree: Box::new(Scope::new("__@")),
        }
    }
}

// Scope Tree Node can only have one parent and could have multiple children
#[derive(Debug)]
pub struct Scope {
    pub root: bool,
    pub scope_name: String,
    pub symbols: HashMap<SymbolName, SymbolInfo>,
    // child scope cannot make changes on parent scope
    pub parent: Option<Weak<Scope>>,
    // child scope might be changed
    pub children: RefCell<Vec<Rc<RefCell<Scope>>>>,
}

impl Scope {
    pub fn new(scope_name: &str) -> Self {
        Scope {
            root: false,
            // Default scope name set in the first node
            scope_name: scope_name.to_string(),
            symbols: HashMap::new(),
            parent: None,
            children: RefCell::new(vec![]),
        }
    }

    pub fn add_child(&self, child: Rc<RefCell<Scope>>) -> InternalResult<()> {
        // If there is no parent scope found, return error
        match &child.borrow_mut().parent {
            Some(_parent) => { /*found parent scope */ }
            None => return Err(InternalError::ParentScopeNotFoundError),
        }
        self.children.borrow_mut().push(child);
        Ok(())
    }

    pub fn set_parent(&mut self, parent: Weak<Scope>){
        self.parent = Some(parent);
    }

    pub fn add_symbol(&mut self, symbol_name: &str, symbol_info: SymbolInfo) {
        self.symbols.insert(symbol_name.to_string(), symbol_info);
    }

    pub fn get_symbol_info(&self, symbol_name: &str) -> &SymbolInfo {
        self.symbols
            .get(symbol_name)
            .unwrap_or_else(|| panic!("Faild to get a symbol {}", symbol_name))
    }
}

// For one symbol
#[derive(Debug)]
pub struct SymbolInfo {
    // Data type used in RuLa
    pub data_type: Types,
}
