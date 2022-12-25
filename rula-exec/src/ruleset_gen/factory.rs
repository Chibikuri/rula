use proc_macro2::TokenStream;

pub fn generate_factory() -> TokenStream {
    quote!(
    #[derive(Debug, PartialEq, Clone)]
    pub struct RuleSetFactory {
        pub repeaters: RepeaterList,
        pub rule_generators: HashMap<String, RuleGenerators>,
        pub rule_arguments: HashMap<String, RuleArgs>,
        // Order of vector is very important
        pub unconverted_rule_arguments: HashMap<String, Vec<RuLaValue>>,
        pub rulesets: Vec<RuleSet>
    }


    impl RuleSetFactory{
        pub fn from(repeaters: Vec<Repeater>) -> Self {
            let mut rulesets = vec![];
            for i in 0..repeaters.len(){
                rulesets.push(RuleSet::new("empty_ruleset"))
            }
            RuleSetFactory {
                repeaters: RepeaterList::from(repeaters),
                rule_generators: HashMap::new(),
                rule_arguments: HashMap::new(),
                unconverted_rule_arguments: HashMap::new(),
                rulesets: rulesets,
            }
        }
        pub fn len(&self) -> usize {
            self.repeaters.len()
        }

        pub fn update_ruleset_name(&mut self, new_name: &str){
            for rs in &mut self.rulesets{
                rs.update_name(new_name);
            }
        }

        pub fn add_rule_generator(&mut self, rule_name: &str, rule_generator: RuleGenerators){
            self.rule_generators.insert(rule_name.to_string(), rule_generator);
        }

        pub fn genenerate_stage(&mut self, rule_name: &str, repeater_index: usize){
            let stage = self.rule_generators.get_mut(rule_name).expect("Failed to find the rule").generate(self.repeaters.at(repeater_index), self.rule_arguments.get(rule_name).expect("Failed to get arguments"));
            self.rulesets[repeater_index].add_stage(stage);
        }
        // just register arguments not resolved
        pub fn register_args(&mut self, rule_name: &str, arguments: Vec<RuLaValue>){
            self.unconverted_rule_arguments.insert(rule_name.to_string(), arguments);
        }

        pub fn resolve_args(&mut self, rule_name: &str){
            let generators = self.rule_generators.get(rule_name).expect("Failed to get rule generator");
            let actual_args = self.unconverted_rule_arguments.get(rule_name).expect("Failed to get inserted values");
            let arg_names = generators.get_rule_args();
            if arg_names.len() != actual_args.len(){
                panic!("the number of arguments doesn't match");
            }
            let mut rule_arg = RuleArgs::new();
            for (arg_n, arg_v) in arg_names.iter().zip(actual_args.iter()){
                rule_arg.set(arg_n, arg_v.clone());
            }
            self.rule_arguments.insert(rule_name.to_string(), rule_arg);
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
