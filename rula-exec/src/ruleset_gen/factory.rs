use proc_macro2::TokenStream;

pub fn generate_factory() -> TokenStream {
    quote!(
    #[derive(Debug, PartialEq, Clone)]
    pub struct RuleSetFactory {
        pub repeaters: RepeaterList,
        pub rule_generators: RefCell<HashMap<String, RuleGenerators>>,
        pub rule_arguments: RefCell<HashMap<String, RuleArgs>>,
        // Order of vector is very important
        pub unconverted_rule_arguments: RefCell<HashMap<String, Vec<RuLaValue>>>,
        pub rulesets: RefCell<Vec<RefCell<RuleSet>>>
    }


    impl RuleSetFactory{
        pub fn from(repeaters: Vec<Repeater>) -> Self {
            let mut rulesets = vec![];
            for i in 0..repeaters.len(){
                rulesets.push(RefCell::new(RuleSet::new("empty_ruleset")))
            }
            RuleSetFactory {
                repeaters: RepeaterList::from(repeaters),
                rule_generators: RefCell::new(HashMap::new()),
                rule_arguments: RefCell::new(HashMap::new()),
                unconverted_rule_arguments: RefCell::new(HashMap::new()),
                rulesets: RefCell::new(rulesets),
            }
        }
        pub fn len(&self) -> usize {
            self.repeaters.len()
        }

        pub fn update_ruleset_name(&self, new_name: &str){
            for rs in self.rulesets.borrow().iter(){
                rs.borrow_mut().update_name(new_name);
            }
        }

        pub fn add_rule_generator(&self, rule_name: &str, rule_generator: RuleGenerators){
            self.rule_generators.borrow_mut().insert(rule_name.to_string(), rule_generator);
        }

        pub fn genenerate_stage(&self, rule_name: &str, repeater_index: usize){
            let stage = self.rule_generators.borrow_mut().get_mut(rule_name).expect("Failed to find the rule").generate(self.repeaters.at(repeater_index), self.rule_arguments.borrow().get(rule_name).expect("Failed to get arguments"));
            self.rulesets.borrow_mut()[repeater_index].borrow_mut().add_stage(stage);
        }
        // just register arguments not resolved
        pub fn register_args(&self, rule_name: &str, arguments: Vec<RuLaValue>){
            self.unconverted_rule_arguments.borrow_mut().insert(rule_name.to_string(), arguments);
        }

        pub fn resolve_args(&self, rule_name: &str){
            let rule_generators = self.rule_generators.borrow();
            let generators = rule_generators.get(rule_name).expect("Failed to get rule generator");
            let uncovered_rule_args = self.unconverted_rule_arguments.borrow();
            let actual_args = uncovered_rule_args.get(rule_name).expect("Failed to get inserted values");
            let arg_names = generators.get_rule_args();
            if arg_names.len() != actual_args.len(){
                panic!("the number of arguments doesn't match");
            }
            let mut rule_arg = RuleArgs::new();
            for (arg_n, arg_v) in arg_names.iter().zip(actual_args.iter()){
                rule_arg.set(arg_n, arg_v.clone());
            }
            self.rule_arguments.borrow_mut().insert(rule_name.to_string(), rule_arg);
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct RepeaterList{
        pub reps: Vec<Repeater>,
    }

    impl RepeaterList{
        pub fn from(repeaters: Vec<Repeater>)->Self{
            RepeaterList { reps: repeaters }
        }
        pub fn len(&self) -> usize{
            self.reps.len()
        }
        pub fn at(&self, index: usize) -> &Repeater {
            &self.reps[index]
        }
    })
}
