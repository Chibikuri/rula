use serde_json::Value;

use super::ruleset::{RuleSet, Stage};
use super::ruleset_generator::{Scope, ValueTracker};
use super::types::{Repeater, RuLaValue, Types};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

type NodeNumber = usize;
type RuleName = String;

// Track all global state in generation
// #[derive(Debug)]
pub struct Tracker {
    // pub rulesets: RefCell<HashMap<NodeNumber, RuleSet>>,
    pub ruleset_name: String,
    pub rule_names: HashSet<String>,
    pub internal_repeater_name: HashMap<RuleName, String>,
    pub rule_argument_names: HashMap<RuleName, Vec<(String, Types)>>,
    pub return_type_annotation: HashMap<RuleName, RetTypeAnnotation>,
    pub num_repeater: u32,
    // Hashmap<Scope Name, <Identifier Name, Types>>
    pub variables: HashMap<String, HashMap<String, Types>>,
    // Different from ordinary variables
    // This map only contains the variables that is set by set expressions
    pub set_variables: HashMap<String, Types>,
}

impl Tracker {
    pub fn new() -> Self {
        Tracker {
            // rulesets: RefCell::new(HashMap::new()),
            ruleset_name: String::from(""),
            rule_names: HashSet::new(),
            internal_repeater_name: HashMap::new(),
            rule_argument_names: HashMap::new(),
            return_type_annotation: HashMap::new(),
            num_repeater: 0,
            variables: HashMap::new(),
            set_variables: HashMap::new(),
        }
    }

    // Functions for Rules
    pub fn check_rule_name_exist(&self, name: &str) -> bool {
        self.rule_names.contains(name)
    }

    pub fn register_rule_name(&mut self, name: &str) {
        if !self.check_rule_name_exist(name) {
            self.rule_names.insert(name.to_string());
        }
    }

    pub fn add_internal_repeater_name(&mut self, rule_name: &RuleName, repeater_arg_name: &String) {
        self.internal_repeater_name
            .insert(rule_name.to_string(), repeater_arg_name.to_string());
    }

    pub fn get_internal_repeater_name(&self, rule_name: &RuleName) -> String {
        self.internal_repeater_name
            .get(rule_name)
            .expect("Unable to find the rule name")
            .to_string()
    }

    pub fn add_rule_arguments(
        &mut self,
        rule_name: &RuleName,
        argument_names: Vec<(String, Types)>,
    ) {
        self.rule_argument_names
            .insert(rule_name.to_string(), argument_names);
    }

    pub fn get_rule_arguments(&self, rule_name: &RuleName) -> &Vec<(String, Types)> {
        self.rule_argument_names
            .get(rule_name)
            .expect("Failed to get argument names")
    }

    pub fn check_rule_arguments(&self, rule_name: &RuleName, arg_name: &String) -> (bool, Types) {
        if !self.check_rule_name_exist(rule_name) {
            return (false, Types::Unknown);
        } else {
            for (name, type_def) in self.get_rule_arguments(rule_name) {
                if name == arg_name {
                    return (true, type_def.clone());
                }
            }
            (false, Types::Unknown)
        }
    }

    pub fn add_return_type_annotation(
        &mut self,
        rule_name: &RuleName,
        ret_type_annotation: RetTypeAnnotation,
    ) {
        self.return_type_annotation
            .insert(rule_name.to_string(), ret_type_annotation);
    }

    pub fn exist_return_types(&self, rule_name: &RuleName) -> bool {
        self.return_type_annotation.contains_key(rule_name)
    }

    pub fn check_return_type_annotations(&self, rule_name: &RuleName) -> &RetTypeAnnotation {
        if !self.exist_return_types(rule_name) {
            panic!("No return type annotation found!");
        }
        self.return_type_annotation
            .get(rule_name)
            .expect("Failed to get")
    }

    pub fn update_num_node(&mut self, num_node: u32) {
        self.num_repeater = num_node;
    }

    pub fn register_variable(&mut self, ident_name: &String, type_hint: &Types, scope: &String) {
        if !self.variables.contains_key(scope) {
            let mut new_map = HashMap::new();
            new_map.insert(ident_name.to_string(), type_hint.clone());
            self.variables.insert(scope.to_string(), new_map);
        } else {
            if self
                .variables
                .get(scope)
                .expect("Failed to get scope name")
                .contains_key(ident_name)
            {
                panic!("Variable {} is already defined in {}", ident_name, scope);
            } else {
                self.variables
                    .get_mut(scope)
                    .expect("Failed to get scope name")
                    .insert(ident_name.to_string(), type_hint.clone());
            }
        }
    }

    pub fn check_variable_exist(&self, ident_name: &String, scope: &String) -> bool {
        if !self.variables.contains_key(scope) {
            panic!("No variable found in {}", scope);
        }

        if !self
            .variables
            .get(scope)
            .expect("Failed to get scope")
            .contains_key(ident_name)
        {
            panic!(
                "No variable {} in {}, DEBUG {:#?}'",
                ident_name, scope, self.variables
            )
        }

        self.variables
            .get(scope)
            .expect("Failed to get scope")
            .contains_key(ident_name)
    }

    pub fn get_variable_type_hint(&self, ident_name: &String, scope: &String) -> &Types {
        self.check_variable_exist(ident_name, scope);

        self.variables
            .get(scope)
            .expect("Failed to get scope")
            .get(ident_name)
            .expect("Failed to get identifier")
    }

    pub fn register_set_variable(&mut self, ident_name: &String, type_hint: &Types) {
        self.set_variables
            .entry(ident_name.to_string())
            .or_insert(type_hint.clone());
    }

    pub fn get_type_hint_of_set_variable(&self, ident_name: &String) -> &Types {
        self.set_variables
            .get(ident_name)
            .expect("Failed to get type hint")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: String,
    // Currently, this scope is the same as Rule name and RuleSet name
    pub scope: String,
    pub variable_kind: VarKind,
    pub value: Types,
}

impl Variable {
    pub fn new(name: &str, scope: &str, variable_kind: VarKind, value: Types) -> Self {
        Variable {
            name: name.to_string(),
            scope: scope.to_string(),
            variable_kind: variable_kind,
            value: value,
        }
    }
    pub fn get_scope(&self) -> &String {
        &self.scope
    }
}

// Variable Types with scope
#[derive(Debug, Clone, PartialEq)]
pub enum VarKind {
    Argument,
    RepeaterArgument,
    LetAssignment,
    // Initialization purpose
    Nil,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RetTypeAnnotation {
    // (Type annotation, maybe)
    pub return_types: Vec<(Types, bool)>,
    pub scope: String,
}

impl RetTypeAnnotation {
    pub fn new(scope: &str) -> Self {
        RetTypeAnnotation {
            return_types: vec![],
            scope: scope.to_string(),
        }
    }
    pub fn add_return_type(&mut self, ret_type: Types, maybe: bool) {
        self.return_types.push((ret_type, maybe))
    }
    pub fn exist(&self) -> bool {
        self.return_types.len() > 0
    }
    pub fn get_scope(&self) -> &String {
        &self.scope
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arguments {
    // Order is important here
    pub vals: Vec<RuLaValue>,
    pub scope: String,
}

impl Arguments {
    pub fn new(vals: Vec<RuLaValue>, scope: String) -> Self {
        Arguments {
            vals: vals,
            scope: scope,
        }
    }
    pub fn place_holder() -> Self {
        Arguments {
            vals: vec![],
            scope: String::from(""),
        }
    }
    pub fn get_scope(&self) -> &String {
        &self.scope
    }
    pub fn add_val(&mut self, arg: RuLaValue) {
        self.vals.push(arg);
    }
    pub fn update_scope(&mut self, new_scope: &str) {
        self.scope = new_scope.to_string();
    }
}

// Used to track inside the rule closure
#[derive(Debug, Clone, PartialEq)]
pub struct LocalVariableTracker {
    pub values: RefCell<HashMap<String, RuLaValue>>,
}

impl LocalVariableTracker {
    pub fn new() -> Self {
        LocalVariableTracker {
            values: RefCell::new(HashMap::new()),
        }
    }
    pub fn register(&self, name: &str, val: &RuLaValue) {
        self.values
            .borrow_mut()
            .insert(name.to_string(), val.clone());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_clean_scope() {
        // let mut tracker = Tracker::new();
        // // Create variables for two scopes
        // let var_1 = Variable::new("variable", "rule1", VarKind::Argument, Types::Unknown);
        // let var_2 = Variable::new("variable2", "rule2", VarKind::Argument, Types::Unknown);
        // tracker.register_local_variable("variable", var_1);
        // tracker.register_local_variable("variable", var_2);
        // // clean variables in rule1
        // tracker.clean_scope("rule1");
        // assert_eq!(tracker.local_variable.borrow().len(), 1);
    }
}
