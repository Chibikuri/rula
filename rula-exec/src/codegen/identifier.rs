// use rula_parser::parser::ast::IdentType;
// use std::{
//     collections::{HashMap, HashSet},
//     vec,
// };

// // This should be fixed in the future
// // If there are two same name variables in the different rules, this would duplicate it.
// #[derive(Debug, Clone, PartialEq)]
// pub struct IdentTracker {
//     pub identifiers: HashMap<String, Identifier>,
//     // Should have better way to track this
//     pub interface_names: HashSet<String>,
//     pub rule_names: Vec<String>,
//     pub config_name: Option<String>,
//     pub num_node: Option<String>,
// }

// impl IdentTracker {
//     pub fn new() -> Self {
//         IdentTracker {
//             identifiers: HashMap::new(),
//             interface_names: HashSet::new(),
//             rule_names: vec![],
//             config_name: None,
//             num_node: None,
//         }
//     }
//     pub fn register(&mut self, ident_name: &str, ident: Identifier) {
//         self.identifiers.insert(ident_name.to_string(), ident);
//     }

//     pub fn update_config_name(&mut self, config_name: &str) {
//         self.config_name = Some(String::from(config_name));
//     }

//     pub fn update_num_node(&mut self, num_node: &str) {
//         self.num_node = Some(String::from(num_node));
//     }

//     pub fn add_interface_name(&mut self, interface_name: &str) {
//         if self.exist_interface(interface_name) {
//             panic!("Interface name cannot be duplicated :{}", interface_name);
//         }
//         self.interface_names.insert(String::from(interface_name));
//     }

//     pub fn exist_interface(&self, interface_name: &str) -> bool {
//         self.interface_names.contains(interface_name)
//     }

//     pub fn add_rule_name(&mut self, rule_name: &str) {
//         if self.exist_rule_name(rule_name) {
//             panic!("Rule name cannot be duplicated");
//         }
//         self.rule_names.push(rule_name.to_string());
//     }

//     pub fn exist_rule_name(&self, rule_name: &str) -> bool {
//         self.rule_names.contains(&String::from(rule_name))
//     }

//     pub fn check_ident_type(&mut self, ident_name: &str) -> IdentType {
//         match self.identifiers.get(ident_name) {
//             Some(ident) => ident.ident_type.clone(),
//             None => IdentType::Other,
//         }
//     }
//     pub fn check_type_hint(&mut self, ident_name: &str) -> TypeHint {
//         match self.identifiers.get(ident_name) {
//             Some(ident) => ident.type_hint.clone(),
//             None => TypeHint::Unknown,
//         }
//     }
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct Identifier {
//     pub ident_type: IdentType,
//     pub type_hint: TypeHint,
// }

// #[derive(Debug, Clone, PartialEq)]
// pub enum TypeHint {
//     Str,
//     Integer64,
//     UnsignedInteger64,
//     Float64,
//     Boolean,
//     Qubit,
//     StrVector,
//     I64Vector,
//     U64Vector,
//     F64Vector,
//     BoolVector,
//     Unknown,
// }

// impl Identifier {
//     pub fn new(ident_type: IdentType, type_hint: TypeHint) -> Self {
//         Identifier {
//             ident_type: ident_type,
//             type_hint: type_hint,
//         }
//     }
// }
