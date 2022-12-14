use proc_macro2::TokenStream;

pub fn generate_factory() -> TokenStream {
    quote!(
    #[derive(Debug, PartialEq, Clone)]
    pub struct RuleSetFactory {
        pub repeaters: RepeaterList,
        pub rule_generators: HashMap<String, RuleGenerators>,
        pub rule_arguments: HashMap<String, RuleArgs>,
    }


    impl RuleSetFactory{
        pub fn from(repeaters: Vec<Repeater>) -> Self {
            RuleSetFactory {
                repeaters: RepeaterList::from(repeaters),
                rule_generators: HashMap::new(),
                rule_arguments: HashMap::new(),
            }
        }
        pub fn len(&self) -> usize {
            self.repeaters.len()
        }

        pub fn add_rule_generator(&mut self, rule_name: &str, rule_generator: RuleGenerators){
            self.rule_generators.insert(rule_name.to_string(), rule_generator);
        }

        pub fn gen_rule(&mut self, rule_name: &str, repeater_index: usize) -> Stage{
            self.rule_generators.get_mut(rule_name).expect("Failed to find the rule").generate(self.repeaters.at(repeater_index), self.rule_arguments.get(rule_name).expect("Failed to get arguments"))
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
