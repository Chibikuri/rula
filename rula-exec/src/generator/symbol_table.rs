use super::scope::Scope;
use super::types::Types;

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
    use std::rc::Rc;
    use std::cell::RefCell;
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
