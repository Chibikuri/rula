use std::collections::HashMap;

use super::IResult;
use super::error::CompileError;
use super::symbol_table::SymbolInfo;
use std::cell::RefCell;
use std::rc::{Rc, Weak};

type SymbolName = String;

// Scope Tree Node can only have one parent and could have multiple children
#[derive(Debug)]
pub struct Scope {
    pub root: bool,
    pub scope_name: String,
    pub symbols: HashMap<SymbolName, SymbolInfo>,
    // child scope cannot make changes on parent scope
    pub parent: Option<Weak<RefCell<Scope>>>,
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

    pub fn set_root(&mut self) {
        match &self.parent {
            Some(_parent) => {
                panic!("A scope that has parent scope cannot be a root scope")
            }
            None => {
                self.root = true;
            }
        }
    }

    pub fn add_child(&self, child: Rc<RefCell<Scope>>) -> IResult<()> {
        // If there is no parent scope found, return error
        match &child.borrow_mut().parent {
            // Parent information must be set before added
            Some(_parent) => { /*found parent scope */ }
            None => return Err(CompileError::ParentScopeNotFoundError),
        }
        self.children.borrow_mut().push(child);
        Ok(())
    }

    pub fn set_parent(&mut self, parent: Weak<RefCell<Scope>>) {
        self.parent = Some(parent);
    }

    pub fn ref_parent(&self) -> IResult<Option<Rc<RefCell<Scope>>>> {
        match &self.parent {
            Some(parent_ref) => {
                // check if the parent is still alive
                match parent_ref.upgrade() {
                    Some(parent) => Ok(Some(parent)),
                    None => Err(CompileError::ParentScopeDroppedError),
                }
            }
            // Root node
            None => Ok(None),
        }
    }

    pub fn add_symbol(&mut self, symbol_name: &str, symbol_info: SymbolInfo) {
        self.symbols.insert(symbol_name.to_string(), symbol_info);
    }

    pub fn get_symbol_info(&self, symbol_name: &str) -> Option<SymbolInfo> {
        // found in the current scope
        match self.symbols.get(symbol_name) {
            Some(symbol_info) => Some(symbol_info.clone()),
            None => {
                // get parent node
                // FIXME: Don't wanna clone
                match self.ref_parent().unwrap() {
                    Some(parent_node) => {
                        let symbol = parent_node
                            .borrow()
                            .get_symbol_info(symbol_name)
                            .expect("Failed to find the symbol")
                            .clone();
                        return Some(symbol);
                    }
                    None => {
                        // No more parent here
                        return None;
                    }
                }
            }
        }
    }
}