// use std::collections::HashMap;
// use std::collections::HashSet;
// use std::sync::Mutex;

// use super::rule_meta::*;
// use super::IResult;

// use crate::rulep::action::v2::ActionClauses;
// use crate::rulep::action::Action;
// use crate::rulep::condition::v1::ConditionClauses;
// use crate::rulep::ruleset::{Rule, RuleSet};
// use crate::wrapper::qnic_wrapper::QnicInterfaceWrapper;

// use rula_parser::parser::ast::*;

// // Right now, compilation is all single thread. In the future, this could be multi thread.
// // Mutex would be a good choice for that.
// // type MutexRuleSet = Mutex<RuleSet<ActionClauses>>;
// // static RULESET: OnceCell<MutexRuleSet> = OnceCell::new();

// #[derive(Debug, Clone, PartialEq)]
// pub struct RuleInfo {
//     pub meta: RuleMeta,
//     pub rule: Rule<ActionClauses>,
//     pub qnic_interfaces: HashSet<String>,
//     // pub qnic_interfaces: HashMap<String, QnicInterfaceWrapper>,
// }

// impl RuleInfo {
//     pub fn new(
//         rule_meta: RuleMeta,
//         rule: Rule<ActionClauses>,
//         qnic_interfaces: HashSet<String>,
//     ) -> Self {
//         RuleInfo {
//             meta: rule_meta,
//             rule: rule,
//             qnic_interfaces: qnic_interfaces,
//         }
//     }
//     pub fn add_assigned_interface(&mut self, interface_name: &str) {
//         self.qnic_interfaces.insert(interface_name.to_string());
//     }

//     pub fn exist_interface(&mut self, interface_name: &str) -> bool {
//         self.qnic_interfaces.contains(interface_name)
//     }

//     pub fn add_watch_value(&mut self, value_name: &str, watchable: Watchable) {
//         self.meta
//             .insert_watch_value(String::from(value_name), watchable)
//     }
//     pub fn exist_watched_value(&mut self, value_name: &str) -> bool {
//         self.meta.watched_values.contains_key(value_name)
//     }

//     pub fn add_rule_arg(&mut self, arg: &str) {
//         self.meta.add_rule_arg(arg);
//     }
//     pub fn add_condition_clause(&mut self, clause: ConditionClauses) {
//         self.rule.condition.add_condition_clause(clause);
//     }

//     pub fn update_quantum_prop(&mut self, value: &str, quantum_prop: QuantumProp) {
//         self.meta
//             .watched_values
//             .get_mut(value)
//             .unwrap()
//             .update_quantum_prop(quantum_prop);
//     }
// }

// // This struct is used to create RuleSet during the code generation.
// // This struct implements helper functions to refer interface and current rule information
// #[derive(Debug, Clone, PartialEq)]
// pub struct RuleSetFactory {
//     // Table of Rules
//     pub rule_table: HashMap<String, RuleInfo>,
//     // Global interfaces that includes all the possible interfaces
//     pub global_interfaces: HashMap<String, QnicInterfaceWrapper>,
// }

// impl RuleSetFactory {
//     pub fn init() -> Self {
//         RuleSetFactory {
//             rule_table: HashMap::new(),
//             global_interfaces: HashMap::new(),
//         }
//     }
//     pub fn assign_qnic_to_rule(&mut self, rule_name: &str, interface_name: &str) {
//         // Check if the interface exists
//         if !self.exist_interface(interface_name) {
//             panic!("No interface name {} found", interface_name);
//         }
//         // Assign qnic to rule
//         self.rule_table
//             .get_mut(rule_name)
//             .unwrap()
//             .add_assigned_interface(interface_name);
//     }

//     pub fn exist_assigned_interface(&mut self, rule_name: &str, interface_name: &str) -> bool {
//         self.rule_table
//             .get_mut(rule_name)
//             .expect("Unable to get corresponding RuleInfo")
//             .exist_interface(interface_name)
//     }

//     pub fn add_global_interface(&mut self, interface_name: &str, interface: QnicInterfaceWrapper) {
//         self.global_interfaces
//             .insert(String::from(interface_name), interface);
//     }

//     pub fn add_watch_value(&mut self, rule_name: &str, value: &str, watchable: Watchable) {
//         self.rule_table
//             .get_mut(rule_name)
//             .unwrap()
//             .add_watch_value(value, watchable);
//     }

//     pub fn add_rule_arg(&mut self, rule_name: &str, value: &str) {
//         self.rule_table
//             .get_mut(rule_name)
//             .unwrap()
//             .add_rule_arg(value);
//     }

//     pub fn exist_rule(&mut self, rule_name: &str) -> bool {
//         self.rule_table.contains_key(rule_name)
//     }

//     pub fn get_rule(&mut self, rule_name: &str) -> Rule<ActionClauses> {
//         self.rule_table.get(rule_name).unwrap().rule.clone()
//     }

//     pub fn exist_interface(&mut self, interface_name: &str) -> bool {
//         self.global_interfaces.contains_key(interface_name)
//     }

//     pub fn exist_watched_value(&mut self, rule_name: &str, value_name: &str) -> bool {
//         self.rule_table
//             .get_mut(rule_name)
//             .unwrap()
//             .exist_watched_value(value_name)
//     }

//     // This should return reference in the future
//     pub fn get_interface(&mut self, interface_name: &str) -> QnicInterfaceWrapper {
//         if self.exist_interface(interface_name) {
//             return self.global_interfaces.get(interface_name).unwrap().clone();
//         } else {
//             panic!("No interface named {} found", interface_name);
//         }
//     }

//     pub fn add_rule(&mut self, rule_name: &str, rule: Rule<ActionClauses>) {
//         let rule_info = RuleInfo::new(RuleMeta::place_holder(), rule, HashSet::new());
//         self.rule_table.insert(rule_name.to_string(), rule_info);
//     }

//     pub fn add_condition_clause(&mut self, rule_name: &str, condition_clause: ConditionClauses) {
//         self.rule_table
//             .get_mut(rule_name)
//             .unwrap()
//             .add_condition_clause(condition_clause);
//     }

//     pub fn update_watched_value(
//         &mut self,
//         rule_name: &str,
//         value: &str,
//         quantum_prop: QuantumProp,
//     ) {
//         self.rule_table
//             .get_mut(rule_name)
//             .unwrap()
//             .update_quantum_prop(value, quantum_prop);
//     }
// }

// pub fn generate_ruleset(ruleset: &RuleSetExpr) -> IResult<RuleSet<Action<ActionClauses>>> {
//     // generate ruleset from rule_expr
//     let ruleset_name = &*ruleset.name.name;
//     let _default_rule = match &*ruleset.default {
//         Some(default_rule) => default_rule,
//         None => todo!("No default rule!"),
//     };
//     let rules = &*ruleset.rules;
//     for _stmt in rules {
//         // statement
//         // match &*stmt.kind {
//         //     // get return value from rule
//         //     StmtKind::Let(let_stmt) => {
//         //         // This info might not be used at this moment
//         //         let return_val_store = &*let_stmt.ident;
//         //         let rule_expr = match &*let_stmt.expr.kind {
//         //             ExprKind::FnCall(rule_name) => &*rule_name.func_name,
//         //             _ => todo!("error"),
//         //         };
//         //     }
//         //     StmtKind::Expr(expr) => {}
//         //     _ => todo!("Here ruleset suppose to be a set of rules."),
//         // }
//     }
//     // let rule_table = vec![];

//     let ruleset = RuleSet::new(ruleset_name);
//     Ok(ruleset)
// }

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::rulep::ruleset::Rule;

//     #[test]
//     fn test_make_json() {
//         // let ruleset = generate_ruleset();
//     }

//     #[test]
//     fn test_generate_simple_ruleset() {
//         // ruleset entanglement_swapping{
//         //     default: default()
//         //     let q1_entangled = swapping();
//         //     pauli_correction(q1_entangled)
//         // }
//         // let test_ruleset = RuleSetExpr::new(
//         //     Ident::new("entanglement_swapping", None),
//         //     None,
//         //     Some(FnCall::new(Ident::new("default", None), vec![])),
//         //     vec![
//         //         RuleIdentifier::Let(Let::new(
//         //             Ident::new("q1_entangled", None),
//         //             Expr::new(ExprKind::FnCall(FnCall::new(
//         //                 Ident::new("swapping", None),
//         //                 vec![],
//         //             ))),
//         //         )),
//         //         RuleIdentifier::FnCall(FnCall::new(
//         //             Ident::new("pauli_correction", None),
//         //             vec![Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(
//         //                 Ident::new("q1_entangled", None),
//         //             ))))],
//         //         )),
//         //     ],
//         // );

//         // let mut target_ruleset = RuleSet::<Action<ActionClauses>>::new("entanglement_swapping");
//         // target_ruleset.add_rule(Rule::<Action<ActionClauses>>::new("swappping"));
//         // target_ruleset.add_rule(Rule::<Action<ActionClauses>>::new("pauli_correction"));
//         // let _generated_ruleset = generate_ruleset(&test_ruleset).unwrap();
//         // assert_eq!(target_ruleset, generated_ruleset);
//         // assert_eq!(&generated_ruleset.rules[0].name, "swapping");
//         // assert_eq!(&generated_ruleset.rules[1].name, "pauli_correction");
//     }
// }
