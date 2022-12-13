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
    pub rulesets: RefCell<HashMap<NodeNumber, RuleSet>>,
    pub ruleset_name: String,
    pub rule_names: HashSet<String>,
    pub internal_repeater_name: HashMap<RuleName, String>,
    pub internal_argument_names: HashMap<RuleName, Vec<String>>,
    pub return_type_annotation: HashMap<RuleName, RetTypeAnnotation>,
    pub num_repeater: u32,
}

impl Tracker {
    pub fn new() -> Self {
        Tracker {
            rulesets: RefCell::new(HashMap::new()),
            ruleset_name: String::from(""),
            rule_names: HashSet::new(),
            internal_repeater_name: HashMap::new(),
            internal_argument_names: HashMap::new(),
            return_type_annotation: HashMap::new(),
            num_repeater: 0,
        }
    }

    // Functions for RuleSet
    pub fn add_ruleset(&self, number: NodeNumber, ruleset: RuleSet) {
        self.rulesets.borrow_mut().insert(number, ruleset);
    }

    pub fn update_ruleset_name(&mut self, new_name: &str) {
        self.ruleset_name = new_name.to_string();
        for (_, ruleset) in self.rulesets.borrow_mut().iter_mut() {
            ruleset.update_name(new_name);
        }
    }

    pub fn add_stage(&self, repeater_index: usize, stage: Stage) {
        self.rulesets
            .borrow_mut()
            .get_mut(&repeater_index)
            .expect("Failed to find a ruelset")
            .add_stage(stage);
    }

    pub fn return_rulesets(&self) -> Vec<RuleSet> {
        let mut rulesets = vec![];
        for (_, rs) in self.rulesets.borrow().iter() {
            rulesets.push(rs.clone())
        }
        rulesets
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

    pub fn add_internal_argument_names(
        &mut self,
        rule_name: &RuleName,
        argument_names: Vec<String>,
    ) {
        self.internal_argument_names
            .insert(rule_name.to_string(), argument_names);
    }

    pub fn get_internal_argument_names(&self, rule_name: &RuleName) -> &Vec<String> {
        self.internal_argument_names
            .get(rule_name)
            .expect("Failed to get argument names")
    }

    pub fn add_return_type_annotation(
        &mut self,
        rule_name: &RuleName,
        ret_type_annotation: RetTypeAnnotation,
    ) {
        self.return_type_annotation
            .insert(rule_name.to_string(), ret_type_annotation);
    }

    pub fn update_num_node(&mut self, num_node: u32) {
        self.num_repeater = num_node;
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

type ArgName = String;
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
