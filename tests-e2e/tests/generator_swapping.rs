use itertools::Itertools;
use rula_exec::ruleset_gen::action::*;
use rula_exec::ruleset_gen::condition::*;
use rula_exec::ruleset_gen::conf_parser;
use rula_exec::ruleset_gen::ruleset::*;
use rula_exec::ruleset_gen::types::*;
use rula_lib as rula_std;
use rula_std::prelude::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::rc::Rc;
#[allow(unused)]
mod rula {
    use super::*;
    type RepeaterNumber = usize;
    type RuleVec = Rc<RefCell<Vec<RefCell<Rule>>>>;
    type FactoryType = Rc<RefCell<RuleSetFactory>>;
    type RuleName = String;
    use rula_exec::ruleset_gen::types::Repeater;
    #[derive(Debug, Clone, PartialEq)]
    pub struct RuleArgs {
        pub args: HashMap<String, RuLaValue>,
    }
    impl RuleArgs {
        pub fn new() -> Self {
            RuleArgs {
                args: HashMap::new(),
            }
        }
        pub fn get(&self, arg_name: &str) -> &RuLaValue {
            self.args
                .get(arg_name)
                .expect("Failed to find the argument")
        }
        pub fn set(&mut self, arg_name: &str, value: RuLaValue) {
            self.args.insert(arg_name.to_string(), value);
        }
    }
    #[derive(Debug, Clone, PartialEq)]
    pub enum RuleGenerators {
        __swapping(__swapping),
    }
    impl RuleGenerators {
        pub fn generate(&mut self, _repeater: &Repeater, _arguments: &RuleArgs) -> Stage {
            match self {
                RuleGenerators::__swapping(_rule) => _rule.gen_rules(_repeater, _arguments),
            }
        }
        pub fn get_rule_args(&self) -> Vec<String> {
            match self {
                RuleGenerators::__swapping(_rule) => _rule.get_rule_args(),
            }
        }
    }
    #[derive(Debug, PartialEq, Clone)]
    pub struct RuleSetFactory {
        pub repeaters: RepeaterList,
        pub rule_generators: RefCell<HashMap<RuleName, RuleGenerators>>,
        pub rule_arguments: RefCell<HashMap<RuleName, RuleArgs>>,
        pub promoted_values: RefCell<HashMap<RuleName, Vec<RuLaValue>>>,
        pub unconverted_rule_arguments: RefCell<HashMap<String, Vec<RuLaValue>>>,
        pub rulesets: RefCell<Vec<RefCell<RuleSet>>>,
        pub set_values: RefCell<HashMap<String, RuLaValue>>,
    }
    impl RuleSetFactory {
        pub fn from(repeaters: Vec<Repeater>) -> Self {
            let mut rulesets = vec![];
            for i in 0..repeaters.len() {
                rulesets.push(RefCell::new(RuleSet::new("empty_ruleset")))
            }
            RuleSetFactory {
                repeaters: RepeaterList::from(repeaters),
                rule_generators: RefCell::new(HashMap::new()),
                rule_arguments: RefCell::new(HashMap::new()),
                promoted_values: RefCell::new(HashMap::new()),
                unconverted_rule_arguments: RefCell::new(HashMap::new()),
                rulesets: RefCell::new(rulesets),
                set_values: RefCell::new(HashMap::new()),
            }
        }
        pub fn len(&self) -> usize {
            self.repeaters.len()
        }
        pub fn update_ruleset_name(&self, new_name: &str) {
            for rs in self.rulesets.borrow().iter() {
                rs.borrow_mut().update_name(new_name);
            }
        }
        pub fn add_rule_generator(&self, rule_name: &str, rule_generator: RuleGenerators) {
            self.rule_generators
                .borrow_mut()
                .insert(rule_name.to_string(), rule_generator);
        }
        pub fn genenerate_stage(&self, rule_name: &str, repeater_index: usize) {
            let stage = self
                .rule_generators
                .borrow_mut()
                .get_mut(rule_name)
                .expect("Failed to find the rule")
                .generate(
                    self.repeaters.at(repeater_index),
                    self.rule_arguments
                        .borrow()
                        .get(rule_name)
                        .expect("Failed to get arguments"),
                );
            self.rulesets.borrow_mut()[repeater_index]
                .borrow_mut()
                .add_stage(stage);
        }
        pub fn register_args(&self, rule_name: &str, arguments: Vec<RuLaValue>) {
            self.unconverted_rule_arguments
                .borrow_mut()
                .insert(rule_name.to_string(), arguments);
        }
        pub fn resolve_args(&self, rule_name: &str) {
            let rule_generators = self.rule_generators.borrow();
            let generators = rule_generators
                .get(rule_name)
                .expect("Failed to get rule generator");
            let uncovered_rule_args = self.unconverted_rule_arguments.borrow();
            let actual_args = uncovered_rule_args
                .get(rule_name)
                .expect("Failed to get inserted values");
            let arg_names = generators.get_rule_args();
            if arg_names.len() != actual_args.len() {
                panic!("the number of arguments doesn't match");
            }
            let mut rule_arg = RuleArgs::new();
            for (arg_n, arg_v) in arg_names.iter().zip(actual_args.iter()) {
                rule_arg.set(arg_n, arg_v.clone());
            }
            self.rule_arguments
                .borrow_mut()
                .insert(rule_name.to_string(), rule_arg);
        }
        pub fn promote(&self, rules: RuleVec, rule_name: &str, value: RuLaValue) {
            let identifier = match &value {
                RuLaValue::Qubit(qubit) => QubitIdentifier {
                    qubit_index: qubit.index,
                },
                _ => {
                    todo!("Currently, the only value that can be promoted is qubit");
                }
            };
            for rule in rules.borrow().iter() {
                rule.borrow_mut()
                    .add_action_clause(ActionClauses::Promote(identifier.clone()));
            }
            if self.promoted_values.borrow().contains_key(rule_name) {
                self.promoted_values
                    .borrow_mut()
                    .get_mut(rule_name)
                    .expect("")
                    .push(value);
            } else {
                let initial_vec = vec![value];
                self.promoted_values
                    .borrow_mut()
                    .insert(rule_name.to_string(), initial_vec);
            }
        }
        pub fn promoted_values(&self, rule_name: &str) -> Vec<RuLaValue> {
            self.promoted_values
                .borrow()
                .get(rule_name)
                .expect("Failed to get promoted values")
                .clone()
        }
        pub fn set(&self, rules: RuleVec, variable: RuLaValue, name: &str) {
            self.set_values
                .borrow_mut()
                .entry(name.to_string())
                .or_insert(variable);
        }
        pub fn get(&self, rules: RuleVec, name: &str) -> RuLaValue {
            if !self.set_values.borrow().contains_key(name) {
                panic!("No variable found {}", name)
            }
            self.set_values
                .borrow()
                .get(name)
                .expect("Failed to get value")
                .clone()
        }
    }
    #[derive(Debug, Clone, PartialEq)]
    pub struct RepeaterList {
        pub reps: Vec<Repeater>,
    }
    impl RepeaterList {
        pub fn from(repeaters: Vec<Repeater>) -> Self {
            RepeaterList { reps: repeaters }
        }
        pub fn len(&self) -> usize {
            self.reps.len()
        }
        pub fn at(&self, index: usize) -> &Repeater {
            &self.reps[index]
        }
    }
    use rula_std::operation::bsm;
    use rula_std::operation::x;
    use rula_std::operation::z;
    #[derive(Debug, Clone, PartialEq)]
    pub struct __swapping {
        pub name: String,
        pub rule_args: Vec<String>,
        pub callback: Rc<RefCell<RuleSetFactory>>,
    }
    impl __swapping {
        pub fn new(ruleset_factory: Rc<RefCell<RuleSetFactory>>) -> Self {
            __swapping {
                name: "swapping".to_string(),
                rule_args: vec!["distance".to_string()],
                callback: ruleset_factory,
            }
        }
        pub fn gen_rules(&mut self, repeater: &Repeater, arguments: &RuleArgs) -> Stage {
            let initial_rules = Rc::new(RefCell::new(vec![RefCell::new(Rule::new("swapping"))]));
            let generated = self._generate(Rc::clone(&initial_rules), repeater, arguments);
            let mut stage = Stage::new();
            for rule in generated.borrow().iter() {
                stage.add_rule(rule.borrow().clone())
            }
            stage
        }
        pub fn get_rule_args(&self) -> Vec<String> {
            self.rule_args.clone()
        }
        fn _generate(
            &mut self,
            rules: RuleVec,
            __repeater: &Repeater,
            __argument: &RuleArgs,
        ) -> RuleVec {
            let __partner_rules: Rc<RefCell<HashMap<u64, RuleVec>>> =
                Rc::new(RefCell::new(HashMap::new()));
            let left_partner: &Repeater = __repeater.hop(
                Rc::clone(&rules),
                &-__argument.get("distance").eval_as_int(),
            );
            let right_partner: &Repeater =
                __repeater.hop(Rc::clone(&rules), &__argument.get("distance").eval_as_int());
            let condition = Condition::new(None);
            for rule in rules.borrow().iter() {
                rule.borrow_mut().set_condition(condition.clone());
            }
            let q1 = res(Rc::clone(&rules), &1, &0.8, &left_partner, &0);
            let q2 = res(Rc::clone(&rules), &1, &0.8, &right_partner, &1);
            let result: RuLaResult = bsm(Rc::clone(&rules), &q1, &q2);
            let __cmp_target = result.comparable();
            let current_rule_count = rules.borrow().len();
            let final_rule_count = current_rule_count * 5;
            let __match_actions: Vec<
                Box<dyn Fn(RuleVec, HashMap<u64, RuleVec>) -> ((RuleVec, HashMap<u64, RuleVec>))>,
            > = vec![
                Box::new(|__new_rule, mut __new_partner_rules| {
                    let mut __t_rules = vec![];
                    for _temp_rule in __new_rule.borrow().iter() {
                        _temp_rule
                            .borrow_mut()
                            .add_condition_clause(ConditionClauses::Cmp(Cmp::new(
                                CmpOp::Eq,
                                __cmp_target("00"),
                            )));
                        __t_rules.push(_temp_rule.clone());
                    }
                    let __temp_rules = Rc::new(RefCell::new(__t_rules));
                    (__temp_rules, __new_partner_rules)
                }),
                Box::new(|__new_rule, mut __new_partner_rules| {
                    let mut __t_rules = vec![];
                    for _temp_rule in __new_rule.borrow().iter() {
                        _temp_rule
                            .borrow_mut()
                            .add_condition_clause(ConditionClauses::Cmp(Cmp::new(
                                CmpOp::Eq,
                                __cmp_target("01"),
                            )));
                        __t_rules.push(_temp_rule.clone());
                    }
                    let __temp_rules = Rc::new(RefCell::new(__t_rules));
                    if !__new_partner_rules.contains_key(&left_partner.index) {
                        __new_partner_rules.insert(
                            left_partner.index,
                            Rc::new(RefCell::new(vec![RefCell::new(Rule::new("Wait"))])),
                        );
                    }
                    send(
                        Rc::clone(&__temp_rules),
                        &left_partner,
                        ProtoMessageType::Update,
                    );
                    wait(
                        Rc::clone(
                            __new_partner_rules
                                .get(&left_partner.index)
                                .expect("Failed to get corresponding rule vector"),
                        ),
                        &left_partner,
                        ProtoMessageType::Update,
                    );
                    (__temp_rules, __new_partner_rules)
                }),
                Box::new(|__new_rule, mut __new_partner_rules| {
                    let mut __t_rules = vec![];
                    for _temp_rule in __new_rule.borrow().iter() {
                        _temp_rule
                            .borrow_mut()
                            .add_condition_clause(ConditionClauses::Cmp(Cmp::new(
                                CmpOp::Eq,
                                __cmp_target("10"),
                            )));
                        __t_rules.push(_temp_rule.clone());
                    }
                    let __temp_rules = Rc::new(RefCell::new(__t_rules));
                    if !__new_partner_rules.contains_key(&right_partner.index) {
                        __new_partner_rules.insert(
                            right_partner.index,
                            Rc::new(RefCell::new(vec![RefCell::new(Rule::new("Wait"))])),
                        );
                    }
                    send(
                        Rc::clone(&__temp_rules),
                        &right_partner,
                        ProtoMessageType::Update,
                    );
                    wait(
                        Rc::clone(
                            __new_partner_rules
                                .get(&right_partner.index)
                                .expect("Failed to get corresponding rule vector"),
                        ),
                        &right_partner,
                        ProtoMessageType::Update,
                    );
                    (__temp_rules, __new_partner_rules)
                }),
                Box::new(|__new_rule, mut __new_partner_rules| {
                    let mut __t_rules = vec![];
                    for _temp_rule in __new_rule.borrow().iter() {
                        _temp_rule
                            .borrow_mut()
                            .add_condition_clause(ConditionClauses::Cmp(Cmp::new(
                                CmpOp::Eq,
                                __cmp_target("11"),
                            )));
                        __t_rules.push(_temp_rule.clone());
                    }
                    let __temp_rules = Rc::new(RefCell::new(__t_rules));
                    if !__new_partner_rules.contains_key(&left_partner.index) {
                        __new_partner_rules.insert(
                            left_partner.index,
                            Rc::new(RefCell::new(vec![RefCell::new(Rule::new("Wait"))])),
                        );
                    }
                    send(
                        Rc::clone(&__temp_rules),
                        &left_partner,
                        ProtoMessageType::Update,
                    );
                    wait(
                        Rc::clone(
                            __new_partner_rules
                                .get(&left_partner.index)
                                .expect("Failed to get corresponding rule vector"),
                        ),
                        &left_partner,
                        ProtoMessageType::Update,
                    );
                    if !__new_partner_rules.contains_key(&right_partner.index) {
                        __new_partner_rules.insert(
                            right_partner.index,
                            Rc::new(RefCell::new(vec![RefCell::new(Rule::new("Wait"))])),
                        );
                    }
                    send(
                        Rc::clone(&__temp_rules),
                        &right_partner,
                        ProtoMessageType::Update,
                    );
                    wait(
                        Rc::clone(
                            __new_partner_rules
                                .get(&right_partner.index)
                                .expect("Failed to get corresponding rule vector"),
                        ),
                        &right_partner,
                        ProtoMessageType::Update,
                    );
                    (__temp_rules, __new_partner_rules)
                }),
            ];
            let cloned_rules = rules.clone();
            let cloned_partner_rules = __partner_rules.borrow().clone();
            let mut new_rule_vec = vec![];
            let mut new_partner_map: HashMap<u64, RuleVec> = HashMap::new();
            for __action_func in &__match_actions {
                let (generated_rules, generated_partner_rules) =
                    __action_func(cloned_rules.clone(), cloned_partner_rules.clone());
                for gen_rule in &*generated_rules.borrow() {
                    new_rule_vec.push(gen_rule.clone());
                }
                for (rep_id, rule_vec) in generated_partner_rules.into_iter() {
                    if __partner_rules.borrow().contains_key(&rep_id) {
                        for generated_partner_rule in rule_vec.borrow().iter() {
                            if !__partner_rules
                                .borrow()
                                .get(&rep_id)
                                .expect("Failed to get repeater rules")
                                .borrow()
                                .contains(generated_partner_rule)
                            {
                                __partner_rules
                                    .borrow()
                                    .get(&rep_id)
                                    .expect("Failed to get repeater rules")
                                    .borrow_mut()
                                    .push(generated_partner_rule.clone());
                            }
                        }
                    } else {
                        __partner_rules.borrow_mut().insert(rep_id, rule_vec);
                    }
                }
            }
            let mut rules = Rc::new(RefCell::new(vec![]));
            for new_rule in new_rule_vec {
                rules.borrow_mut().push(new_rule);
            }
            let (generated_vec, generated_partner_map) =
                (|__new_rule: RuleVec, mut __new_partner_rules: HashMap<u64, RuleVec>| {
                    let mut __t_rules = vec![];
                    for _temp_rule in __new_rule.borrow().iter() {
                        _temp_rule
                            .borrow_mut()
                            .add_condition_clause(ConditionClauses::Cmp(Cmp::new(
                                CmpOp::Neq,
                                __cmp_target("00"),
                            )));
                        _temp_rule
                            .borrow_mut()
                            .add_condition_clause(ConditionClauses::Cmp(Cmp::new(
                                CmpOp::Neq,
                                __cmp_target("01"),
                            )));
                        _temp_rule
                            .borrow_mut()
                            .add_condition_clause(ConditionClauses::Cmp(Cmp::new(
                                CmpOp::Neq,
                                __cmp_target("10"),
                            )));
                        _temp_rule
                            .borrow_mut()
                            .add_condition_clause(ConditionClauses::Cmp(Cmp::new(
                                CmpOp::Neq,
                                __cmp_target("11"),
                            )));
                        __t_rules.push(_temp_rule.clone());
                    }
                    let __temp_rules = Rc::new(RefCell::new(__t_rules));
                    if !__new_partner_rules.contains_key(&left_partner.index) {
                        __new_partner_rules.insert(
                            left_partner.index,
                            Rc::new(RefCell::new(vec![RefCell::new(Rule::new("Wait"))])),
                        );
                    }
                    send(
                        Rc::clone(&__temp_rules),
                        &left_partner,
                        ProtoMessageType::Free,
                    );
                    wait(
                        Rc::clone(
                            __new_partner_rules
                                .get(&left_partner.index)
                                .expect("Failed to get corresponding rule vector"),
                        ),
                        &left_partner,
                        ProtoMessageType::Free,
                    );
                    if !__new_partner_rules.contains_key(&right_partner.index) {
                        __new_partner_rules.insert(
                            right_partner.index,
                            Rc::new(RefCell::new(vec![RefCell::new(Rule::new("Wait"))])),
                        );
                    }
                    send(
                        Rc::clone(&__temp_rules),
                        &right_partner,
                        ProtoMessageType::Free,
                    );
                    wait(
                        Rc::clone(
                            __new_partner_rules
                                .get(&right_partner.index)
                                .expect("Failed to get corresponding rule vector"),
                        ),
                        &right_partner,
                        ProtoMessageType::Free,
                    );
                    (__temp_rules, __new_partner_rules)
                })(cloned_rules.clone(), cloned_partner_rules.clone());
            for gen_rule in generated_vec.borrow().iter() {
                rules.borrow_mut().push(gen_rule.clone())
            }
            for (rep_id, rule_vec) in generated_partner_map.into_iter() {
                if __partner_rules.borrow().contains_key(&rep_id) {
                    for generated_partner_rule in rule_vec.borrow().iter() {
                        if !__partner_rules
                            .borrow()
                            .get(&rep_id)
                            .expect("Failed to get repeater rules")
                            .borrow()
                            .contains(generated_partner_rule)
                        {
                            __partner_rules
                                .borrow()
                                .get(&rep_id)
                                .expect("Failed to get repeater rules")
                                .borrow_mut()
                                .push(generated_partner_rule.clone());
                        } else {
                            __partner_rules
                                .borrow_mut()
                                .insert(rep_id, rule_vec.clone());
                        }
                    }
                }
            }
            let num_rules = rules.borrow().len();
            if num_rules != final_rule_count {
                panic!(
                    "The final rule size is wrong Suppose: {} != Actual{}",
                    final_rule_count, num_rules
                );
            };
            if !__partner_rules.borrow().contains_key(&left_partner.index) {
                __partner_rules.borrow_mut().insert(
                    left_partner.index,
                    Rc::new(RefCell::new(vec![RefCell::new(Rule::new("Wait"))])),
                );
            }
            send(Rc::clone(&rules), &left_partner, ProtoMessageType::Transfer);
            wait(
                Rc::clone(
                    __partner_rules
                        .borrow()
                        .get(&left_partner.index)
                        .expect("Failed to get the partner rule vector"),
                ),
                &left_partner,
                ProtoMessageType::Transfer,
            );
            if !__partner_rules.borrow().contains_key(&right_partner.index) {
                __partner_rules.borrow_mut().insert(
                    right_partner.index,
                    Rc::new(RefCell::new(vec![RefCell::new(Rule::new("Wait"))])),
                );
            }
            send(
                Rc::clone(&rules),
                &right_partner,
                ProtoMessageType::Transfer,
            );
            wait(
                Rc::clone(
                    __partner_rules
                        .borrow()
                        .get(&right_partner.index)
                        .expect("Failed to get the partner rule vector"),
                ),
                &right_partner,
                ProtoMessageType::Transfer,
            );
            for (partner_index, rules) in __partner_rules.borrow().iter() {
                let mut stage = Stage::new();
                for rule in rules.borrow().iter() {
                    stage.add_rule(rule.borrow().clone());
                }
                self.callback.borrow().rulesets.borrow()[partner_index.clone() as usize]
                    .borrow_mut()
                    .add_stage(stage);
            }
            rules
        }
    }
    pub fn generate_ruleset(__factory: Rc<RefCell<RuleSetFactory>>) -> Vec<RuleSet> {
        __factory
            .borrow()
            .update_ruleset_name("entanglement_swapping");
        for d in 1..(__factory.borrow().repeaters.len() / 2) {
            for i in 1..__factory.borrow().repeaters.len() - 1 {
                if i % (2 * d) == d {
                    (|__ruleset_factory: Rc<RefCell<RuleSetFactory>>| {
                        __ruleset_factory
                            .borrow()
                            .register_args("swapping", vec![RuLaValue::Int(d as i64)]);
                        __ruleset_factory.borrow().resolve_args("swapping");
                        __ruleset_factory.borrow().genenerate_stage("swapping", i);
                    })(Rc::clone(&__factory));
                };
            }
        }
        let mut rulesets = vec![];
        for rs in __factory.borrow().rulesets.borrow().iter() {
            rulesets.push(rs.borrow().clone());
        }
        rulesets
    }
}
pub fn generate_ruleset() {
    let repeaters = conf_parser::parse_config(&"../examples/v2/config.json".into()).unwrap();
    let ruleset_factory = Rc::new(RefCell::new(rula::RuleSetFactory::from(repeaters)));
    let __rule = rula::__swapping::new(Rc::clone(&ruleset_factory));
    ruleset_factory
        .borrow_mut()
        .add_rule_generator("swapping", rula::RuleGenerators::__swapping(__rule));
    let rulesets = rula::generate_ruleset(Rc::clone(&ruleset_factory));
    for (i, ruleset) in rulesets.iter().enumerate() {
        let output_file_path = format!("tests/generated/test_{}.json", i);
        let mut file = File::create(output_file_path).expect("Failed to create a new file");
        let json_ruleset = serde_json::to_string(ruleset).unwrap();
        write!(&file, "{}", json_ruleset).unwrap();
        file.flush().expect("Failed to write");
        println!("{}", json_ruleset);
    }
}
fn main() {
    generate_ruleset()
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_rule_generate() {
        generate_ruleset();
        assert_eq!(1, 2);
    }
}
