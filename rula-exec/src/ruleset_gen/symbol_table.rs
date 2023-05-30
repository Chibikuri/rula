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

    pub fn add_child(&self, child: Rc<RefCell<Scope>>) -> InternalResult<()> {
        // If there is no parent scope found, return error
        match &child.borrow_mut().parent {
            // Parent information must be set before added
            Some(_parent) => { /*found parent scope */ }
            None => return Err(InternalError::ParentScopeNotFoundError),
        }
        self.children.borrow_mut().push(child);
        Ok(())
    }

    pub fn set_parent(&mut self, parent: Weak<RefCell<Scope>>) {
        self.parent = Some(parent);
    }

    pub fn ref_parent(&self) -> InternalResult<Option<Rc<RefCell<Scope>>>> {
        match &self.parent {
            Some(parent_ref) => {
                // check if the parent is still alive
                match parent_ref.upgrade() {
                    Some(parent) => Ok(Some(parent)),
                    None => Err(InternalError::ParentScopeDroppedError),
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

// For one symbol
#[derive(Debug, Clone)]
pub struct SymbolInfo {
    // Data type used in RuLa
    pub data_type: Types,
}

impl SymbolInfo {
    pub fn new() -> Self {
        SymbolInfo {
            data_type: Types::Unknown,
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_scope_tree_creation() {
        // parent scope
        let parent = Rc::new(RefCell::new(Scope::new("__@parent")));
        // child scope
        let child = Rc::new(RefCell::new(Scope::new("__@child")));
        // register parent scope
        child.borrow_mut().set_parent(Rc::downgrade(&parent));

        // Check if it's possible to refer to the parent from child
        let parent_ref = match child.borrow().ref_parent().unwrap() {
            Some(paref) => paref,
            None => {
                panic!("No parents found");
            }
        };
        assert_eq!(parent_ref.borrow().scope_name, String::from("__@parent"));
    }

    #[test]
    fn test_scope_tree_look_up() {
        // climb up to root to find a value
        // root scope
        let root = Rc::new(RefCell::new(Scope::new("__@root")));
        root.borrow_mut()
            .add_symbol("root_symbol", SymbolInfo::new());
        // child scope
        let child = Rc::new(RefCell::new(Scope::new("__@child")));
        // grandchild
        let grand_child = Rc::new(RefCell::new(Scope::new("__@grand_child")));

        grand_child.borrow_mut().set_parent(Rc::downgrade(&child));
        child.borrow_mut().set_parent(Rc::downgrade(&root));

        let root_symbol = child
            .borrow_mut()
            .get_symbol_info("root_symbol")
            .expect("Failed to get symbol");
        assert_eq!(root_symbol.data_type, Types::Unknown);
    }
}
