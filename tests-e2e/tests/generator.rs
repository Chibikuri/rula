use rula_exec::ruleset_gen::action::*;
use rula_exec::ruleset_gen::condition::*;
use rula_exec::ruleset_gen::conf_parser;
use rula_exec::ruleset_gen::ruleset::{Rule, RuleSet, Stage};
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
        pub rule_generators: HashMap<String, RuleGenerators>,
        pub rule_arguments: HashMap<String, RuleArgs>,
        pub unconverted_rule_arguments: HashMap<String, Vec<RuLaValue>>,
    }
    impl RuleSetFactory {
        pub fn from(repeaters: Vec<Repeater>) -> Self {
            RuleSetFactory {
                repeaters: RepeaterList::from(repeaters),
                rule_generators: HashMap::new(),
                rule_arguments: HashMap::new(),
                unconverted_rule_arguments: HashMap::new(),
            }
        }
        pub fn len(&self) -> usize {
            self.repeaters.len()
        }
        pub fn add_rule_generator(&mut self, rule_name: &str, rule_generator: RuleGenerators) {
            self.rule_generators
                .insert(rule_name.to_string(), rule_generator);
        }
        pub fn genenerate_stage(&mut self, rule_name: &str, repeater_index: usize) -> Stage {
            self.rule_generators
                .get_mut(rule_name)
                .expect("Failed to find the rule")
                .generate(
                    self.repeaters.at(repeater_index),
                    self.rule_arguments
                        .get(rule_name)
                        .expect("Failed to get arguments"),
                )
        }
        pub fn register_args(&mut self, rule_name: &str, arguments: Vec<RuLaValue>) {
            self.unconverted_rule_arguments
                .insert(rule_name.to_string(), arguments);
        }
        pub fn resolve_args(&mut self, rule_name: &str) {
            let generators = self
                .rule_generators
                .get(rule_name)
                .expect("Failed to get rule generator");
            let actual_args = self
                .unconverted_rule_arguments
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
            self.rule_arguments.insert(rule_name.to_string(), rule_arg);
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
        pub rule_args: Vec<String>,
    }
    impl __swapping {
        pub fn new() -> Self {
            __swapping {
                rule_args: vec!["distance".to_string()],
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
        fn _generate(
            &mut self,
            rules: RuleVec,
            __repeater: &Repeater,
            __argument: &RuleArgs,
        ) -> RuleVec {
            let left_partner: &Repeater =
                __repeater.hop(Rc::clone(&rules), __argument.get("distance").eval_as_int());
            let right_partner: &Repeater =
                __repeater.hop(Rc::clone(&rules), -__argument.get("distance").eval_as_int());
            let mut condition = Condition::new(None);
            let q1 = res(Rc::clone(&rules), 1, 0.8, left_partner, 0);
            let q2 = res(Rc::clone(&rules), 1, 0.8, right_partner, 1);
            for rule in rules.borrow().iter() {
                rule.borrow_mut().set_condition(condition.clone());
            }
            let result: RuLaResult = bsm(Rc::clone(&rules), q1, q2);
            let __cmp_target = result.comparable();
            let current_rule_count = rules.borrow().len();
            let final_rule_count = current_rule_count * 5;
            let __match_conditions = vec![
                (CmpOp::Eq, __cmp_target("00")),
                (CmpOp::Eq, __cmp_target("01")),
                (CmpOp::Eq, __cmp_target("10")),
                (CmpOp::Eq, __cmp_target("11")),
            ];
            let __match_actions: Vec<Box<dyn Fn(Rule) -> (RuleVec)>> = vec![
                Box::new(|__new_rule| {
                    let __temp_rules = Rc::new(RefCell::new(vec![RefCell::new(__new_rule)]));
                    __temp_rules
                }),
                Box::new(|__new_rule| {
                    let __temp_rules = Rc::new(RefCell::new(vec![RefCell::new(__new_rule)]));
                    send(
                        Rc::clone(&__temp_rules),
                        &left_partner,
                        ProtoMessageType::Update,
                    );
                    __temp_rules
                }),
                Box::new(|__new_rule| {
                    let __temp_rules = Rc::new(RefCell::new(vec![RefCell::new(__new_rule)]));
                    send(
                        Rc::clone(&__temp_rules),
                        &right_partner,
                        ProtoMessageType::Update,
                    );
                    __temp_rules
                }),
                Box::new(|__new_rule| {
                    let __temp_rules = Rc::new(RefCell::new(vec![RefCell::new(__new_rule)]));
                    send(
                        Rc::clone(&__temp_rules),
                        &left_partner,
                        ProtoMessageType::Update,
                    );
                    send(
                        Rc::clone(&__temp_rules),
                        &right_partner,
                        ProtoMessageType::Update,
                    );
                    __temp_rules
                }),
            ];
            let finally_rules = &*rules.clone();
            let mut new_rule_vec = vec![];
            for rule in &*rules.borrow_mut() {
                for ((__cmp_op, __val), __action_func) in
                    &mut __match_conditions.iter().zip(&__match_actions)
                {
                    let mut cloned_rule = rule.borrow().clone();
                    cloned_rule.add_condition_clause(ConditionClauses::Cmp(Cmp::new(
                        __cmp_op.clone(),
                        __val.clone(),
                    )));
                    let generated_rules = __action_func(cloned_rule);
                    for gen_rule in &*generated_rules.borrow() {
                        new_rule_vec.push(gen_rule.clone());
                    }
                }
            }
            let mut rules = Rc::new(RefCell::new(vec![]));
            for new_rule in new_rule_vec {
                rules.borrow_mut().push(new_rule);
            }
            let mut fin_rule_stack = vec![];
            for fin_rule in &*finally_rules.borrow_mut() {
                let generated_vec = (|__new_rule| {
                    let __temp_rules = Rc::new(RefCell::new(vec![RefCell::new(__new_rule)]));
                    send(
                        Rc::clone(&__temp_rules),
                        &left_partner,
                        ProtoMessageType::Free,
                    );
                    send(
                        Rc::clone(&__temp_rules),
                        &right_partner,
                        ProtoMessageType::Free,
                    );
                    __temp_rules
                })(fin_rule.borrow().clone());
                for gen_rule in &*generated_vec.borrow() {
                    fin_rule_stack.push(gen_rule.clone());
                }
            }
            for finally_rule in &fin_rule_stack {
                rules.borrow_mut().push(finally_rule.clone());
            }
            let num_rules = rules.borrow().len();
            if num_rules != final_rule_count {
                panic!(
                    "The final rule size is wrong Suppose: {} != Actual{}",
                    final_rule_count, num_rules
                );
            }
            send(Rc::clone(&rules), &left_partner, ProtoMessageType::Transfer);
            send(
                Rc::clone(&rules),
                &right_partner,
                ProtoMessageType::Transfer,
            );
            rules
        }
        pub fn get_rule_args(&self) -> Vec<String> {
            self.rule_args.clone()
        }
        fn _type_check() {}
    }
    pub fn generate_ruleset(mut __factory: RuleSetFactory) -> Vec<RuleSet> {
        let mut rulesets = vec![];
        for i in 0..5 {
            rulesets.insert(i, RuleSet::new("entanglement_swapping"));
        }
        for i in 1..__factory.repeaters.len() - 1 {
            if i % 2 == 1 {
                __factory.register_args("swapping", vec![RuLaValue::Int(1)]);
                __factory.resolve_args("swapping");
                let _stage = __factory.genenerate_stage("swapping", i);
                &rulesets[i as usize].add_stage(_stage);
            }
        }
        rulesets
    }
}
pub fn generate_ruleset() {
    let repeaters = conf_parser::parse_config("../examples/v2/config.json".into()).unwrap();
    let mut ruleset_factory = rula::RuleSetFactory::from(repeaters);
    let __rule = rula::__swapping::new();
    ruleset_factory.add_rule_generator("swapping", rula::RuleGenerators::__swapping(__rule));
    let rulesets = rula::generate_ruleset(ruleset_factory);
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
