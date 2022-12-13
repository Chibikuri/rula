use super::ruleset::{Rule, RuleSet, Stage};
use super::ruleset_generator::Closure;
use super::types::{ArgVals, Repeater, Types};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

type NodeNumber = usize;
type RuleName = String;
// Track all global state in generation
// #[derive(Debug)]
pub struct Tracker {
    pub rulesets: RefCell<HashMap<NodeNumber, RuleSet>>,
    pub rule_names: HashSet<String>,
    pub unresolved_rules: HashMap<RuleName, Closure<Repeater, Arguments, Stage>>,

    pub repeaters: Vec<Repeater>,
    // Variables with scope
    pub local_variable: RefCell<HashMap<String, Variable>>,
    pub return_type_annotation: RefCell<HashMap<u32, RetTypeAnnotation>>,
    index: u32,
}

impl Tracker {
    pub fn new() -> Self {
        Tracker {
            rulesets: RefCell::new(HashMap::new()),
            rule_names: HashSet::new(),
            unresolved_rules: HashMap::new(),
            repeaters: vec![],
            local_variable: RefCell::new(HashMap::new()),
            return_type_annotation: RefCell::new(HashMap::new()),
            index: 0,
        }
    }

    // Functions for RuleSet
    pub fn add_ruleset(&self, number: NodeNumber, ruleset: RuleSet) {
        self.rulesets.borrow_mut().insert(number, ruleset);
    }

    pub fn update_ruleset_name(&self, new_name: &str) {
        for (_, ruleset) in self.rulesets.borrow_mut().iter_mut() {
            ruleset.update_name(new_name);
        }
    }

    pub fn add_stage(&self, repeater_index: usize, stage: Stage) {
        self.rulesets
            .borrow_mut()
            .get_mut(&repeater_index)
            .expect("Failed to get ruelset")
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

    pub fn add_unresolved_rule<F>(
        &mut self,
        rule_name: &str,
        rule: Closure<Repeater, Arguments, Stage>,
    ) where
        F: Fn(Repeater, Arguments) -> Stage + 'static,
    {
        if !self.check_rule_name_exist(rule_name) {
            panic!("No rule name registered {}", rule_name);
        }
        self.unresolved_rules.insert(rule_name.to_string(), rule);
    }

    pub fn eval_rule(&self, rule_name: &str, repeater: &Repeater, arguments: &Arguments) -> Stage {
        self.unresolved_rules
            .get(rule_name)
            .expect("unable to find the rule")(repeater.clone(), arguments.clone())
    }

    // Functions for local variables
    pub fn register_local_variable(&self, name: &str, variable: Variable) {
        self.local_variable
            .borrow_mut()
            .insert(name.to_string(), variable);
    }

    pub fn register_return_type_annotation(&mut self, ret_type_annotation: RetTypeAnnotation) {
        self.return_type_annotation
            .borrow_mut()
            .insert(self.index, ret_type_annotation);
        self.index += 1;
    }

    pub fn clean_scope(&mut self, scope_name: &str) {
        self.clean_local_var(scope_name);
        self.clean_ret_annotation(scope_name);
    }

    fn clean_local_var(&mut self, scope_name: &str) {
        // Copy the current local variable map and remove all the scoped values (Better way?)
        let local_vars = self.local_variable.borrow().clone();
        for (name, var) in local_vars.iter() {
            if var.get_scope() == scope_name {
                self.local_variable.borrow_mut().remove(name);
            }
        }
    }

    fn clean_ret_annotation(&mut self, scope_name: &str) {
        let ret_annos = self.return_type_annotation.borrow().clone();
        for (i, anno) in ret_annos.iter() {
            if anno.get_scope() == scope_name {
                self.return_type_annotation.borrow_mut().remove(i);
            }
        }
    }

    pub fn eval_repeater(&mut self, repeater: Repeater) {}
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
    vals: HashMap<String, ArgVals>,
    scope: String,
}

impl Arguments {
    pub fn new(vals: HashMap<String, ArgVals>, scope: String) -> Self {
        Arguments {
            vals: vals,
            scope: scope,
        }
    }
    pub fn place_holder() -> Self {
        Arguments {
            vals: HashMap::new(),
            scope: String::from(""),
        }
    }
    pub fn get_scope(&self) -> &String {
        &self.scope
    }
    pub fn add_val(&mut self, name: &str, arg: ArgVals) {
        self.vals.insert(name.to_string(), arg);
    }
    pub fn update_scope(&mut self, new_scope: &str) {
        self.scope = new_scope.to_string();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_clean_scope() {
        let mut tracker = Tracker::new();
        // Create variables for two scopes
        let var_1 = Variable::new("variable", "rule1", VarKind::Argument, Types::Unknown);
        let var_2 = Variable::new("variable2", "rule2", VarKind::Argument, Types::Unknown);
        tracker.register_local_variable("variable", var_1);
        tracker.register_local_variable("variable", var_2);
        // clean variables in rule1
        tracker.clean_scope("rule1");
        assert_eq!(tracker.local_variable.borrow().len(), 1);
    }
}
