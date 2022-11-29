// This is entory point to generate code from AST
use super::default_token::*;
use super::error::*;
use super::identifier::*;
// use super::ruleset_generator::RuleSetFactory;
use super::IResult;

use rula_parser::parser::ast::*;

use proc_macro2::{Span, TokenStream};
use std::path::PathBuf;
use syn::Lit as SynLit;
use syn::{LitFloat, LitInt, LitStr};

// Right now, compilation is all single thread. In the future, this could be multi thread.
// Mutex would be a good choice for that.
// static RULES: OnceCell<Mutex<Vec<String>>> = OnceCell::new();

/// Generate corresponding rust code from ast
/// Every nested generators returns a piece of TokenStream
/// Arguments:
///     ast_tree
pub fn generate(
    ast_tree: &mut AstNode,
    with_ruleset: bool,
    config_path: Option<PathBuf>,
) -> IResult<TokenStream> {
    // ident_tracker tracks all the identifier especially special values
    // Special values to be tracked
    // - QnicInterfaceName
    // - QubitInterfaceName: for type resolve reason
    // - ConfigName
    // - WatchedValues: To be awaited
    // - RuleArgument: To pass from ruleset
    let mut ident_tracker = IdentTracker::new();

    // Generated rula program
    let rula_program = match ast_tree {
        // All RuLa AST starts RuLa Node
        AstNode::RuLa(rula) => generate_rula(rula, &mut ident_tracker).unwrap(),
        AstNode::PlaceHolder => {
            return Err(RuLaCompileError::RuLaInitializationError(
                InitializationError::new("at very first generation"),
            ))
        }
    };

    // Generate config reader
    let (config_gen, config_arg) = match config_path {
        Some(path) => {
            let config_name = match &ident_tracker.config_name {
                Some(conf_name) => {
                    format_ident!("{}", conf_name)
                }
                None => panic!("Failed to resolve config name"),
            };
            let path_lit = &path.into_os_string().into_string().unwrap();
            (
                quote!(
                    let content = fs::read_to_string(#path_lit).unwrap();
                    let mut config: rula::#config_name = toml::from_str(&content).unwrap();
                    config.__finalize();
                ),
                quote!(config: rula::#config_name),
            )
        }
        None => (quote!(), quote!()),
    };

    // If we need static ruleset, add this part to generate staic ruleset
    let ruleset_gen = if with_ruleset {
        quote!(
            let mut static_ruleset = RuleSet::<ActionClauses>::new("");
            ruleset.gen_ruleset(&mut static_ruleset, i as u32);
            println!("{:#?}", &static_ruleset);
        )
    } else {
        quote!()
    };

    // The number of node written in config
    let num_node = match &ident_tracker.num_node {
        Some(number) => {
            let num = SynLit::Int(LitInt::new(number, Span::call_site()));
            quote!(#num)
        }
        None => {
            let number = SynLit::Int(LitInt::new("1", Span::call_site()));
            quote!(#number)
        }
    };

    // Used to make enums for the different types of rules
    let mut unready_rule_adder = vec![];
    let mut unready_rule_names = vec![];
    let mut ready_rule_names = vec![];
    let mut unready_rule_checker = vec![];
    let mut arg_list_generator = vec![];
    let mut arg_resolvers = vec![];
    let mut ruleset_gen_enum = vec![];

    for rname in ident_tracker.rule_names {
        let rname_unready = format_ident!("unready_{}", rname);
        let rname_ready = format_ident!("{}", rname);
        unready_rule_names.push(quote!(
            #rname_unready(#rname_unready)
        ));
        ready_rule_names.push(quote!(
            #rname_ready(#rname_ready)
        ));

        unready_rule_adder.push(quote!(
            ruleset.add_unready_rule(
                String::from(#rname),
                rula::UnreadyRules::#rname_unready(rula::#rname_unready::new(__static_interface_list.__get_interface(i)))
            )
        ));

        let match_maker = |func_name: TokenStream| {
            quote!(
                UnreadyRules::#rname_unready(#rname_unready) => {
                    #rname_unready.#func_name
                },
            )
        };
        unready_rule_checker.push(match_maker(quote!(argument_resolved())));
        arg_list_generator.push(match_maker(quote!(arg_list())));
        arg_resolvers.push(match_maker(quote!(resolve_argument(arg_name, argument);)));
        ruleset_gen_enum.push(match_maker(quote!(gen_ruleset(ruleset);)));
    }

    // Generate tests for generaetd rust code.
    // This is just returns tokenstream that describes the test contents
    let generated_tests = helper::generate_test();
    let default_imports = default_imports();
    Ok(quote!(
        use rula_lib as rula_std;
        use std::fs;
        use std::fs::File;
        use std::io::Write;
        use crate::rula_std::ruleset::ruleset::*;
        use rula_std::ruleset::action::v2::ActionClauses;
        #[allow(non_snake_case)]
        #[allow(non_camel_case_types)]
        #[allow(unused_doc_comments)]
        mod rula{
            #default_imports
            pub static INTERFACES: OnceCell<TokioMutex<HashMap<String, QnicInterface>>> = OnceCell::new();
            pub enum UnreadyRules{
                #(#unready_rule_names),*
            }
            impl UnreadyRules{
                pub fn check_arg_resolved(&self) -> Option<ReadyRules>{
                    match &self{
                        #(#unready_rule_checker)*
                        _ => {panic!("No rule name found");}
                    }
                }
                pub fn arg_list(&self) -> Vec<String>{
                    match &self{
                        #(#arg_list_generator)*
                        _ => {panic!("No rule name found");}
                    }
                }
                pub fn resolve_argument(&mut self, arg_name: &str, argument: Argument){
                    match self{
                        #(#arg_resolvers)*
                        _ => {panic!("No rule name found");}
                    }
                }
                pub fn gen_ruleset(&mut self, ruleset: &mut RuleSet<ActionClausesV2>){
                    match self{
                        #(#ruleset_gen_enum)*
                        _ => {panic!("No rule name found")}
                    }
                }
            }


            pub enum ReadyRules{
                #(#ready_rule_names),*
            }

            #rula_program
        }

        pub fn __gen_static_rulesets(rulesets: &mut Vec<RuleSet<ActionClauses>>, #config_arg){
            let __static_interface_list = rula::initialize_static_interface(#num_node);
            for i in 0..#num_node{
                let mut ruleset = rula::RuleSetExec::init();
                #(#unready_rule_adder);*;
                ruleset.resolve_config(Box::new(&config), Some(i as usize));
                #ruleset_gen

                let output_file_path = format!("tests/generated/test_{}.json", i);
                let mut file = File::create(output_file_path).expect("Failed to create a new file");
                let json_ruleset = serde_json::to_string(&static_ruleset).unwrap();
                write!(&file, "{}", json_ruleset).unwrap();
                file.flush().expect("Failed to write");

                rulesets.push(static_ruleset);
            }
        }

        pub async fn __execute_ruleset(){
            rula::initialize_interface().await;
        }
        pub async fn main(){
            let mut rulesets = vec![];
            #config_gen
            __gen_static_rulesets(&mut rulesets, config);

        }

        #[cfg(test)]
        mod tests{
            use super::*;
            use super::rula::*;
            #generated_tests
        }
    ))
}

// Generate entire rula program
pub(super) fn generate_rula(
    rula: &mut RuLa,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    match &mut *rula.rula {
        RuLaKind::Program(program) => {
            let generated_program = generate_program(program, ident_tracker).unwrap();
            return Ok(quote!(#generated_program));
        }
        RuLaKind::Ignore => return Ok(quote!()),
        RuLaKind::Eoi => {
            return Ok(quote!(
                #[doc = "End of input"]
            ))
        }
        RuLaKind::PlaceHolder => {
            return Err(RuLaCompileError::RuLaInitializationError(
                InitializationError::new("at generate rula"),
            ))
        }
    }
}

// Generate progra
// program: Program AST that contains a vector of Stmt
pub(super) fn generate_program(
    program: &mut Program,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    // A vector to store a set of generated stmts
    let mut stmts = vec![];
    for program_block in &mut program.programs {
        // Right now, a program can takes a stmt
        match program_block {
            ProgramKind::Stmt(stmt) => {
                stmts.push(generate_stmt(stmt, None, ident_tracker, false, false).unwrap())
            }
        }
    }
    Ok(quote!(#(#stmts )*))
}

// Generate stmt expression
// Arguments:
//  stmt: Stmt AST
//  rule_name: Option<String> a name of corresponding Rule
//  ident_tracker: List of identifiers that need to be tracked
pub(super) fn generate_stmt(
    stmt: &mut Stmt,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
    do_await: bool,
    in_static: bool,
) -> IResult<TokenStream> {
    let generated_stmt = match &mut *stmt.kind {
        StmtKind::Let(let_stmt) => {
            Ok(generate_let(let_stmt, rule_name, ident_tracker, false, in_static).unwrap())
        }
        StmtKind::Interface(interface) => Ok(generate_interface(interface, ident_tracker).unwrap()),
        StmtKind::Config(config) => Ok(generate_config(config, ident_tracker).unwrap()),
        StmtKind::Expr(expr) => {
            Ok(generate_expr(expr, rule_name, ident_tracker, do_await, in_static, false).unwrap())
        }
        StmtKind::PlaceHolder => Err(RuLaCompileError::RuLaInitializationError(
            InitializationError::new("at generate rula"),
        )),
    }
    .unwrap();
    Ok(quote!(#generated_stmt))
}

/// Generate config schema
pub(super) fn generate_config(
    config_stmt: &mut Config,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    // The only keys in config
    let mut conf_keys = vec![];
    // Match arms that allows us to call config by string (name_string => config_value)
    let mut conf_key_match = vec![];
    // Key:Value pairs of config contents
    let mut generated_items = vec![];
    for item in &mut *config_stmt.values {
        generated_items.push(
            generate_config_item(item, ident_tracker, &mut conf_keys, &mut conf_key_match).unwrap(),
        )
    }

    // The name of config that is refered to by the ruleset
    let config_name = generate_ident(&*config_stmt.name, ident_tracker, false, false).unwrap();

    // The number of node in the network
    let num_nodes = SynLit::Int(LitInt::new(&*config_stmt.num_node, Span::call_site()));

    // Track the name of config and the number of nodes in indentifier tracker
    ident_tracker.update_config_name(&*config_stmt.name.name);
    ident_tracker.update_num_node(&*config_stmt.num_node);

    // Generated code
    Ok(quote!(
        // Actual config with additional __names and __num_nodes field
        #[derive(Debug, Serialize, Deserialize)]
        pub struct #config_name{
            #(#generated_items),*,
            __names: Option<HashSet<String>>,
            __num_nodes: Option<u64>,
        }

        // Implementation of this config
        impl #config_name{
            // Add additional values and finalize this config generation
            pub fn __finalize(&mut self){
                let keys = vec![#(#conf_keys.to_string()),*];
                self.__names = Some(HashSet::from_iter(keys.iter().cloned()));
                self.__num_nodes = Some(#num_nodes)
            }

            // Check if the config key exist
            fn __key_exist(&self, value: &str) -> bool{
                self.__names.as_ref().expect("Unable to find config item names").contains(value)

            }

            // If there is a config name exist, return the corresponding value as an Argument type
            pub fn __get_as_arg(&self, config_val: &str, index: Option<usize>) -> Argument{
                if self.__key_exist(config_val){
                    match config_val{
                        #(#conf_key_match),*,
                        _ => {panic!("This should not happen")}
                    }
                }else{
                    panic!("No config value {} found", config_val);
                }
            }
        }
    ))
}

pub(super) fn generate_config_item(
    config_item: &mut ConfigItem,
    ident_tracker: &mut IdentTracker,
    conf_keys: &mut Vec<String>,
    conf_key_match: &mut Vec<TokenStream>,
) -> IResult<TokenStream> {
    if &config_item.value.name[0..1] == "__" {
        panic!("Invalid config name: {}", &config_item.value.name[0..1])
    }
    let conf_key_str = &config_item.value.name;
    let value_type = &*config_item.type_def;
    conf_keys.push(String::from(&*config_item.value.name));

    // Generate config key and type def e.g. value: u32, conf_vector: Vec<u32>
    let value_name = generate_ident(&*config_item.value, ident_tracker, false, false).unwrap();
    let type_def = generate_type_hint(&*config_item.type_def).unwrap();

    // FIXME: shold make another helper function here
    let (arg_val_token, type_hint_token) =
        helper::arg_val_type_token_gen(&Ident::new("", Some(value_type.clone()), IdentType::Other));

    // If this config value is vector, and the index is specified,
    // This argument is expanded
    match value_type {
        TypeDef::Vector(vec_contents) => {
            let (arg_val_token_in_vec, type_hint_token_in_vec) = helper::arg_val_type_token_gen(
                &Ident::new("", Some(*vec_contents.clone()), IdentType::Other),
            );
            conf_key_match.push(
                quote!(
                    #conf_key_str => {
                        let mut arg = Argument::init();
                        match index{
                            Some(ind) => {
                                arg.add_argument(ArgVal::#arg_val_token_in_vec(self.#value_name[ind].clone()), #type_hint_token_in_vec);
                                arg
                            },
                            None => {
                                arg.add_argument(ArgVal::#arg_val_token(self.#value_name.clone()), #type_hint_token);
                                arg
                            }
                        }
                    }
                )
            )
        }
        _ => {
            conf_key_match.push(quote!(
                #conf_key_str => {
                    let mut arg = Argument::init();
                    arg.add_argument(ArgVal::#arg_val_token(self.#value_name), #type_hint_token);
                    arg
                }
            ));
        }
    }

    Ok(quote!(pub #value_name: #type_def))
}

/// Generate 'let' statement
///
/// # Arguments:
///
/// * 'let_stmt' - (Let) Ast node for Let statement
/// * 'rule' - (Rule) Static Rule definition
/// * 'in_watch' - (bool) boolean value if this let statement is in watch or not.
///                 this can be included AST Node later
///
pub(super) fn generate_let(
    let_stmt: &mut Let,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
    in_watch: bool,
    in_static: bool,
) -> IResult<TokenStream> {
    // If this is watched value, register value to the identifier tracker
    let mut do_await = false;
    if in_watch {
        if !in_static {
            do_await = true;
        }
        ident_tracker.register(
            &*let_stmt.ident.name,
            Identifier::new(IdentType::WatchedValue, TypeHint::Unknown),
        );
    }

    // Check type hint to identify
    let identifier = match &*let_stmt.ident.type_hint {
        Some(hint) => {
            match *hint {
                TypeDef::Qubit => ident_tracker.register(
                    &*let_stmt.ident.name,
                    Identifier::new(IdentType::QubitInterface, TypeHint::Qubit),
                ),
                _ => {}
            }
            generate_ident(&*let_stmt.ident, ident_tracker, true, in_static).unwrap()
        }
        None => generate_ident(&*let_stmt.ident, ident_tracker, false, in_static).unwrap(),
    };
    let expr = generate_expr(
        &mut *let_stmt.expr,
        rule_name,
        ident_tracker,
        do_await,
        in_static,
        false,
    )
    .unwrap();

    Ok(quote!(
        let mut #identifier = #expr;
    ))
}

pub(super) fn generate_interface(
    interface_expr: &mut Interface,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let mut interface_names = vec![];

    for i in &mut interface_expr.interface {
        if ident_tracker.exist_interface(&i.name) {
            return Err(RuLaCompileError::InterfaceNameDuplicationError);
        } else {
            ident_tracker.add_interface_name(&i.name);
        }
        ident_tracker.register(
            &i.name,
            Identifier::new(IdentType::QnicInterface, TypeHint::Unknown),
        );
        interface_names.push(SynLit::Str(LitStr::new(&i.name, Span::call_site())));
    }
    let interface_group_name = match &mut *interface_expr.group_name {
        Some(group_name) => {
            if ident_tracker.exist_interface(&*group_name.name) {
                return Err(RuLaCompileError::InterfaceNameDuplicationError);
            } else {
                ident_tracker.add_interface_name(&*group_name.name);
                ident_tracker.register(
                    &*group_name.name,
                    Identifier::new(IdentType::QnicInterface, TypeHint::Unknown),
                );
            }
            &*group_name.name
        }
        None => "",
    };

    interface_names.push(SynLit::Str(LitStr::new(
        interface_group_name,
        Span::call_site(),
    )));
    Ok(quote!(
        pub async fn initialize_interface() {
            assert!(INTERFACES.get().is_none());
            let initialize_interface = || TokioMutex::new(HashMap::new());
            INTERFACES.get_or_init(initialize_interface);
            let interface_list = INTERFACES.get().expect("Failed to get interface");
            for (index, interface_name) in vec![#(#interface_names),*].iter().enumerate() {
                let mock_qnic = QnicInterface::generate_mock_interface(index as u32, interface_name, 10);
                interface_list
                    .lock()
                    .await
                    .insert(interface_name.to_string(), mock_qnic);
            }
        }

        pub fn initialize_static_interface(__num_nodes: u64)-> __StaticInterfaceList{
            let mut __static_interface_list = __StaticInterfaceList::new();
            __static_interface_list.__update_num_node(__num_nodes);
            // Generate mock qnic here
            for _ in 0..__num_nodes{
                let mut __static_interface = __StaticInterface::new();
                for (index, i_name) in vec![#(#interface_names),*].iter().enumerate(){
                    __static_interface.__add_interface_name(i_name);
                    let qnic_interface = QnicInterface::generate_mock_interface(index as u32, i_name, 10);
                    __static_interface.__add_interface(i_name, qnic_interface);
                }
                __static_interface_list.__add_static_interface(__static_interface);
            }
            __static_interface_list.__check();
            __static_interface_list
        }

        type NodeNumber = u64;
        #[derive(Debug, Serialize, Deserialize)]
        pub struct __StaticInterfaceList{
            pub num_nodes: u64,
            pub __static_interfaces: HashMap<NodeNumber, __StaticInterface>,
            index: u64,
        }

        impl __StaticInterfaceList{
            pub fn new() -> Self{
                __StaticInterfaceList { num_nodes: 0, __static_interfaces: HashMap::new(), index: 0}
            }

            pub fn __add_static_interface(&mut self, __static_interface: __StaticInterface){
                self.__static_interfaces.insert(self.index, __static_interface);
                self.index += 1;
            }

            pub fn __get_interface(&self, index: NodeNumber) -> __StaticInterface{
                self.__static_interfaces.get(&index).expect("No interface found").clone()
            }

            pub fn __update_num_node(&mut self, num_nodes: u64){
                self.num_nodes = num_nodes;
            }

            pub fn __check(&self){
                if self.__static_interfaces.len() != self.num_nodes as usize{
                    panic!("The qnics are not properly registered")
                }
            }
        }

        #[derive(Debug, Serialize, Deserialize, Clone)]
        pub struct __StaticInterface{
            pub interface_names: HashSet<String>,
            pub interfaces: HashMap<String, QnicInterface>,
        }

        impl __StaticInterface{
            pub fn new() -> Self{
                __StaticInterface { interface_names: HashSet::new(), interfaces: HashMap::new()}
            }

            pub fn __add_interface_name(&mut self, value: &str){
                if !self.interface_names.contains(value){
                    self.interface_names.insert(value.to_string());
                }else{
                    panic!("Interface name duplication {}", value);
                }
            }

            pub fn __add_interface(&mut self, name: &str, qnic: QnicInterface){
                if self.interface_names.contains(name){
                    self.interfaces.insert(name.to_string(), qnic);
                }else{
                    panic!("No interface found {}", name);
                }
            }

            pub fn __get_interface(&self, interface_name: &str) -> QnicInterface{
                self.interfaces.get(interface_name).expect("Failed to get interface").clone()
            }

        }
    ))
}

pub(super) fn generate_expr(
    expr: &mut Expr,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
    do_await: bool,
    in_static: bool,
    in_closure: bool,
) -> IResult<TokenStream> {
    match &mut *expr.kind {
        ExprKind::Import(import_expr) => Ok(generate_import(import_expr, ident_tracker).unwrap()),
        ExprKind::If(if_expr) => {
            Ok(generate_if(if_expr, ident_tracker, do_await, in_static).unwrap())
        }
        ExprKind::For(for_expr) => Ok(generate_for(for_expr, ident_tracker).unwrap()),
        ExprKind::While(while_expr) => Ok(generate_while(while_expr, ident_tracker).unwrap()),
        ExprKind::FnDef(fn_def_expr) => Ok(generate_fn_def(fn_def_expr, ident_tracker).unwrap()),
        ExprKind::FnCall(fn_call_expr) => Ok(generate_fn_call(
            fn_call_expr,
            rule_name,
            ident_tracker,
            do_await,
            in_static,
            in_closure,
        )
        .unwrap()),
        ExprKind::Struct(struct_expr) => Ok(generate_struct(struct_expr, ident_tracker).unwrap()),
        ExprKind::Return(return_expr) => {
            Ok(generate_return(return_expr, ident_tracker, in_static).unwrap())
        }
        ExprKind::Match(match_expr) => {
            Ok(generate_match(match_expr, ident_tracker, in_static).unwrap())
        }
        ExprKind::Comp(comp_expr) => {
            Ok(generate_comp(comp_expr, rule_name, ident_tracker, do_await, in_static).unwrap())
        }
        ExprKind::RuleSetExpr(ruleset_expr) => {
            Ok(generate_ruleset_expr(ruleset_expr, ident_tracker).unwrap())
        }
        ExprKind::RuleExpr(rule_expr) => Ok(generate_rule(rule_expr, ident_tracker).unwrap()),
        ExprKind::CondExpr(_cond_expr) => {
            todo!("Right now, condition expression cannot be called from expression");
        }
        ExprKind::ActExpr(_act_expr) => {
            todo!("Right now, action expression cannot be called from expression");
        }
        ExprKind::VariableCallExpr(variable_call_expr) => Ok(generate_variable_call(
            variable_call_expr,
            rule_name,
            ident_tracker,
            do_await,
            in_static,
            in_closure,
        )
        .unwrap()),
        ExprKind::Array(array_expr) => Ok(generate_array(&array_expr, ident_tracker).unwrap()),
        ExprKind::Lit(lit_expr) => {
            Ok(generate_lit(&lit_expr, ident_tracker, in_static, false).unwrap())
        }
        ExprKind::Term(term_expr) => Ok(generate_term(*term_expr).unwrap()),
        ExprKind::PlaceHolder => Err(RuLaCompileError::RuLaInitializationError(
            InitializationError::new("at generating expression"),
        )),
    }
}

pub(super) fn generate_import(
    import_expr: &mut Import,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    // Convert a set of paths into a set of identifiers
    let path = import_expr.path.convert_to_ident();
    // Vector to store all paths
    // e.g. [hello::world, good::evening::Yall]
    let mut paths = vec![];
    for path_ident in path.iter() {
        let mut single_path = vec![];
        let quoted_path = if path_ident.name.contains("/") {
            let splitted = path_ident.name.split("/");
            // This is not clean
            for sp in splitted.into_iter() {
                let mut top_path = "";
                if sp == "std" {
                    top_path = "rula_std";
                } else {
                    top_path = sp;
                }
                let new_ident = Ident::new(top_path, None, IdentType::Other);
                let path_fragment =
                    generate_ident(&new_ident, ident_tracker, false, false).unwrap();
                single_path.push(path_fragment)
            }
            let path_head = &single_path[0];
            let path_left = &single_path[1..];
            quote!(
                #path_head #(::#path_left)*
            )
        } else {
            let path_ident = generate_ident(path_ident, ident_tracker, false, false).unwrap();
            quote!(#path_ident)
        };
        paths.push(quoted_path);
    }
    Ok(quote!(
        #(use #paths; )*
    ))
}

pub(super) fn generate_if(
    if_expr: &mut If,
    ident_tracker: &mut IdentTracker,
    do_await: bool,
    in_static: bool,
) -> IResult<TokenStream> {
    // if (block) {expression}
    // (block)
    let first_condition = generate_expr(
        &mut *if_expr.block,
        None,
        ident_tracker,
        do_await,
        in_static,
        false,
    )
    .unwrap();
    let generated_stmt = {
        let mut gen_stmt = vec![];
        for stmt in &mut if_expr.stmts {
            gen_stmt.push(generate_stmt(stmt, None, ident_tracker, do_await, in_static).unwrap());
        }
        gen_stmt
    };

    let generated_els = match &mut *if_expr.els {
        Some(els) => {
            let els_stmt = generate_stmt(els, None, ident_tracker, do_await, in_static).unwrap();
            quote!(
                else{
                    #els_stmt
                }
            )
        }
        None => quote!(),
    };

    if if_expr.elif.len() > 0 {
        let mut elif_quotes = vec![];
        for elif_expr in &mut *if_expr.elif {
            match elif_expr {
                Some(if_expr) => elif_quotes
                    .push(generate_if(if_expr, ident_tracker, do_await, in_static).unwrap()),
                None => {
                    unreachable!()
                }
            }
        }
        Ok(quote!(
            if #first_condition {
                #(#generated_stmt)*
            }#(else #elif_quotes)*
            #generated_els
        ))
    } else {
        Ok(quote!(
            if #first_condition{
                #(#generated_stmt)*
            }#generated_els
        ))
    }
}

pub(super) fn generate_for(
    for_expr: &mut For,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let mut ident_list = vec![];
    for ident in for_expr.pattern.iter() {
        ident_list.push(generate_ident(ident, ident_tracker, false, false).unwrap());
    }
    let generator = generate_expr(
        &mut for_expr.generator,
        None,
        ident_tracker,
        false,
        false,
        false,
    )
    .unwrap();
    let generated = {
        let mut gen_stmt = vec![];
        for stmt in &mut for_expr.stmts {
            gen_stmt.push(generate_stmt(stmt, None, ident_tracker, false, false).unwrap())
        }
        gen_stmt
    };
    if ident_list.len() == 1 {
        let var = &ident_list[0];
        Ok(quote!(
            for #var in #generator {
                #(#generated)*
            }
        ))
    } else if ident_list.len() > 1 {
        Ok(quote!(
            for (#(#ident_list),* ) in generator {
                #(#generated)*
            }
        ))
    } else {
        Err(RuLaCompileError::RuLaInitializationError(
            InitializationError::new("at generating for expression"),
        ))
    }
}

pub(super) fn generate_while(
    while_expr: &mut While,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let block = generate_expr(
        &mut while_expr.block,
        None,
        ident_tracker,
        false,
        false,
        false,
    )
    .unwrap();
    let stmt = generate_stmt(&mut while_expr.stmt, None, ident_tracker, false, false).unwrap();
    Ok(quote!(
        while #block{
            #stmt
        }
    ))
}

pub(super) fn generate_fn_def(
    fn_def_expr: &mut FnDef,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let mut arguments = vec![];
    for ident in fn_def_expr.arguments.iter() {
        match &*ident.type_hint {
            Some(hint) => {
                arguments.push(generate_ident(ident, ident_tracker, true, false).unwrap());
            }
            None => {
                arguments.push(generate_ident(ident, ident_tracker, false, false).unwrap());
            }
        }
    }
    let stmt = generate_stmt(&mut fn_def_expr.stmt, None, ident_tracker, false, false).unwrap();
    // TODO: register this function to function name list and make it checkable
    // Here could have generics in the future
    Ok(quote!(
        pub fn ( #(#arguments),* ) {
            #stmt
        }
    ))
}

pub(super) fn generate_fn_call(
    fn_call_expr: &mut FnCall,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
    do_await: bool,
    in_static: bool,
    in_closure: bool,
) -> IResult<TokenStream> {
    // Before generating functions, check function table to check whether it's properly defined or not
    let mut fn_name = format_ident!("__temp");
    if in_static {
        fn_name = format_ident!("__static__{}", &*fn_call_expr.func_name.name);
    } else {
        fn_name = format_ident!("{}", &*fn_call_expr.func_name.name)
    }
    // let fn_name = generate_ident(&fn_call_expr.func_name, ident_tracker, false).unwrap();
    let generated_arguments = {
        let mut args = vec![];
        for arg in &mut fn_call_expr.arguments {
            let generated_arg =
                generate_expr(arg, rule_name, ident_tracker, do_await, in_static, false).unwrap();
            if in_static {
                args.push(generated_arg);
            } else {
                args.push(quote!(&#generated_arg));
            }
        }
        args
    };
    if in_static {
        if in_closure {
            Ok(quote!(
                #fn_name(Rc::clone(&__temp_rules), #(#generated_arguments.clone()),*)
            ))
        } else {
            Ok(quote!(
                #fn_name(Rc::clone(&rules), #(#generated_arguments.clone()),*)
            ))
        }
    } else {
        if do_await {
            Ok(quote!(
                #fn_name(#(#generated_arguments),*).await
            ))
        } else {
            Ok(quote!(
                #fn_name(#(#generated_arguments),*)
            ))
        }
    }
}

pub(super) fn generate_struct(
    struct_expr: &Struct,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    // todo!("Structure definition would be deprecated");
    let struct_name = generate_ident(&struct_expr.name, ident_tracker, false, false).unwrap();
    let mut struct_items = vec![];
    for ident in struct_expr.items.iter() {
        struct_items.push(generate_ident(ident, ident_tracker, true, false).unwrap());
    }
    Ok(quote!(
        struct #struct_name{
            #(#struct_items),*
        }
    ))
}

// Promote resources to the next stage
pub(super) fn generate_return(
    return_expr: &mut Return,
    ident_tracker: &mut IdentTracker,
    in_static: bool,
) -> IResult<TokenStream> {
    let expr = generate_expr(
        &mut return_expr.target,
        None,
        ident_tracker,
        false,
        in_static,
        false,
    )
    .unwrap();
    Ok(quote!(return #expr))
}

// Generate Match expression to achieve match action
// In the case of static generation, this expand Rules to the stages
pub(super) fn generate_match(
    match_expr: &mut Match,
    ident_tracker: &mut IdentTracker,
    in_static: bool,
) -> IResult<TokenStream> {
    let generated_expr = generate_expr(
        &mut match_expr.expr,
        None,
        ident_tracker,
        false,
        in_static,
        false,
    )
    .unwrap();
    let mut match_arms = vec![];

    let mut match_conditions = vec![];
    let mut match_actions = vec![];
    for arm in &mut match_expr.match_arms {
        if !in_static {
            match_arms.push(generate_match_arm(arm, ident_tracker).unwrap());
        } else {
            let (static_match_condition, static_match_action) =
                generate_static_match_arms(arm, ident_tracker).unwrap();
            match_conditions.push(static_match_condition);
            match_actions.push(static_match_action);
        }
    }

    let finally = match &mut *match_expr.finally {
        Some(fin) => {
            if in_static {
                generate_match_action(fin, ident_tracker, in_static, true).unwrap()
            } else {
                generate_match_action(fin, ident_tracker, in_static, true).unwrap()
            }
        }
        None => quote!(),
    };
    if !in_static {
        match &*match_expr.temp_val {
            Some(value) => {
                let temp_val = generate_ident(value, ident_tracker, false, false).unwrap();
                Ok(quote!(
                    let #temp_val = #generated_expr;
                    match #temp_val{
                        #(#match_arms),*
                        _ => {#finally}
                    }
                ))
            }
            None => Ok(quote!(
                match #generated_expr{
                    #(#match_arms),*
                    _ => {#finally}
                }
            )),
        }
    } else {
        // Right now this only supports CmpOp::Eq
        match &*match_expr.finally {
            Some(_) => {
                Ok(quote!(
                    let (__temp_val, __cmp_kind, __cmp_target) = #generated_expr;
                    let __cmp_conditions = vec![
                        #(#match_conditions),*
                    ];
                    let __cmp_actions:Vec<Box<dyn Fn(Rule<ActionClausesV2>) -> (RuleVec)>> = vec![
                        #(#match_actions),*
                    ];

                    let updated_num_rules = rules.borrow().len() * __cmp_actions.len();

                    let finally_rules = &*rules.clone();
                    let mut new_rule_vec = vec![];
                    for rule in &*rules.borrow_mut(){
                        for ((__cmp_val, __cmp_op, __val), __action_func) in &mut __cmp_conditions.iter().zip(&__cmp_actions){
                            let mut cloned_rule = rule.borrow().clone();
                            cloned_rule.add_condition_clause(ConditionClauses::Cmp(Cmp::new(__cmp_val.clone(), __cmp_op.clone(), __val.clone())));
                            let generated_rules = __action_func(cloned_rule);
                            for gen_rule in &*generated_rules.borrow(){
                                new_rule_vec.push(gen_rule.clone());
                            }
                        }
                    }
                    // Define new rules to flush the current rules
                    let mut rules = Rc::new(RefCell::new(vec![]));
                    for new_rule in new_rule_vec{
                        rules.borrow_mut().push(new_rule);
                    }
                    let num_rules = rules.borrow().len();
                    if  num_rules != updated_num_rules {
                        panic!("The final rule size is wrong Suppose: {} != Actual{}", updated_num_rules, num_rules);
                    }
                    // Add finally
                    let mut fin_rule_stack = vec![];
                    for fin_rule in &*finally_rules.borrow_mut(){
                        let generated_vec = (|__new_rule|{
                            let __temp_rules = Rc::new(RefCell::new(vec![RefCell::new(__new_rule)]));
                            #finally;
                            __temp_rules
                        })(fin_rule.borrow().clone());
                        for gen_rule in &*generated_vec.borrow(){
                            fin_rule_stack.push(gen_rule.clone());
                        }
                    }
                    for finally_rule in &fin_rule_stack{
                        rules.borrow_mut().push(finally_rule.clone());
                    }
                ))
            }
            None => Ok(quote!(
                let (__temp_val, __cmp_kind, __cmp_target) = #generated_expr;
                let __cmp_conditions = vec![
                    #(#match_conditions),*
                ];
                let __cmp_actions:Vec<Box<dyn Fn(Rule<ActionClausesV2>) -> (RuleVec)>> = vec![
                    #(#match_actions),*
                ];

                let updated_num_rules = rules.borrow().len() * __cmp_actions.len();
                let mut new_rule_vec = vec![];
                for rule in &*rules.borrow_mut(){
                    for ((__cmp_val, __cmp_op, __val), __action_func) in &mut __cmp_conditions.iter().zip(&__cmp_actions){
                        let mut cloned_rule = rule.borrow().clone();
                        cloned_rule.add_condition_clause(ConditionClauses::Cmp(Cmp::new(__cmp_val.clone(), __cmp_op.clone(), __val.clone())));
                        let generated_rules = __action_func(cloned_rule);
                        for gen_rule in &*generated_rules.borrow(){
                            new_rule_vec.push(gen_rule.clone());
                        }
                    }
                }
                // Define new rules to flush the current rules
                let mut rules = Rc::new(RefCell::new(vec![]));
                for new_rule in new_rule_vec{
                    rules.borrow_mut().push(new_rule);
                }
                let num_rules = rules.borrow().len();
                if  num_rules != updated_num_rules {
                    panic!("The final rule size is wrong Suppose: {} != Actual{}", updated_num_rules, num_rules);
                }
                // Add finally
            )),
        }
    }
}

pub(super) fn generate_match_arm(
    match_arm: &mut MatchArm,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let generated_match_condition =
        generate_match_condition(&mut *match_arm.condition, ident_tracker).unwrap();
    let generated_match_action =
        generate_match_action(&mut *match_arm.action, ident_tracker, false, false).unwrap();
    Ok(quote!(#generated_match_condition => {#generated_match_action}))
}

pub(super) fn generate_static_match_arms(
    match_arm: &mut MatchArm,
    ident_tracker: &mut IdentTracker,
) -> IResult<(TokenStream, TokenStream)> {
    let generated_match_condition =
        generate_match_condition(&mut *match_arm.condition, ident_tracker).unwrap();
    let generated_match_action =
        generate_match_action(&mut *match_arm.action, ident_tracker, true, true).unwrap();
    Ok((
        quote!(
            (__cmp_kind.clone(), CmpOp::Eq, __cmp_target(#generated_match_condition))
        ),
        quote!(
            Box::new(|__new_rule|{
                let __temp_rules = Rc::new(RefCell::new(vec![RefCell::new(__new_rule)]));
                #generated_match_action;
                __temp_rules
            })
        ),
    ))
}

pub(super) fn generate_match_condition(
    match_condition: &mut MatchCondition,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    // Right now, satisfiable can only take literals, but in the future, this should be more flexible
    match &mut *match_condition.satisfiable {
        Satisfiable::Lit(literal) => Ok(generate_lit(literal, ident_tracker, false, true).unwrap()),
        _ => Err(RuLaCompileError::RuLaInitializationError(
            InitializationError::new("at match condition"),
        )),
    }
}

pub(super) fn generate_match_action(
    match_action: &mut MatchAction,
    ident_tracker: &mut IdentTracker,
    in_static: bool,
    in_closure: bool,
) -> IResult<TokenStream> {
    let mut actionables = vec![];
    for actionable in &mut *match_action.actionable {
        actionables.push(
            generate_expr(actionable, None, ident_tracker, true, in_static, in_closure).unwrap(),
        );
    }
    Ok(quote!(
        #(#actionables);*
    ))
}

pub(super) fn generate_comp(
    comp_expr: &mut Comp,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
    do_await: bool,
    in_static: bool,
) -> IResult<TokenStream> {
    let lhs = generate_expr(
        &mut comp_expr.lhs,
        rule_name,
        ident_tracker,
        do_await,
        in_static,
        false,
    )
    .unwrap();
    let rhs = generate_expr(
        &mut comp_expr.rhs,
        rule_name,
        ident_tracker,
        do_await,
        in_static,
        false,
    )
    .unwrap();
    let (op, cmp_op) = match *comp_expr.comp_op {
        CompOpKind::Lt => (quote!(<), quote!(__CmpOp::Lt)),
        CompOpKind::Gt => (quote!(>), quote!(__CmpOp::Gt)),
        CompOpKind::LtE => (quote!(<=), quote!(__CmpOp::LtE)),
        CompOpKind::GtE => (quote!(>=), quote!(__CmpOp::GtE)),
        CompOpKind::Eq => (quote!(==), quote!(__CmpOp::Eq)),
        CompOpKind::Nq => (quote!(!=), quote!(__CmpOp::Nq)),
        CompOpKind::PlaceHolder => {
            return Err(RuLaCompileError::RuLaInitializationError(
                InitializationError::new("at generating comp expression"),
            ))
        }
    };
    if in_static {
        Ok(quote!(__static__comp(#lhs, #cmp_op, #rhs, Rc::clone(&rules))))
    } else {
        Ok(quote!(__comp(#lhs, #cmp_op, #rhs)))
    }
}

pub(super) fn generate_ruleset_expr(
    ruleset_expr: &RuleSetExpr,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    // 1. Generate static RuleSet for serialized output
    // Get meta information for this RuleSet
    let ruleset_name = &*ruleset_expr.name.name;
    let mut rule_names = vec![];
    for rule in &ruleset_expr.rules {
        match rule {
            // Ordinary Rule call (e.g. swapping_rule())
            RuleIdentifier::FnCall(fn_call_expr) => {
                // Rule without any return values
                rule_names.push(String::from(&*fn_call_expr.func_name.name));
            }
            RuleIdentifier::Let(let_stmt) => {
                // Rule with return value
                // In the static RuleSet, it doesn't care if there is a return value or not
                // The name of the function must be in RULE_TABLE
                let expr = &*let_stmt.expr;
                match &*expr.kind {
                    ExprKind::FnCall(fn_call) => {
                        // rule_add_evaluater(&*fn_call.func_name.name).unwrap();
                        rule_names.push(String::from(&*fn_call.func_name.name));
                    }
                    _ => unreachable!("Invalid RuleCall {:#?}", rule),
                }
            }
        }
    }

    // Replace all the TBD information with configed values
    // Expand Config here
    let config = match &*ruleset_expr.config {
        Some(conf_name) => {
            if Some(String::from(&*conf_name.name)) != ident_tracker.config_name {
                return Err(RuLaCompileError::NoConfigFound);
            }
            let name = generate_ident(conf_name, ident_tracker, false, false).unwrap();
            quote!(
                pub fn resolve_config(&mut self, config: Box<&#name>, index: Option<usize>){
                    for (_, rule) in &mut self.unready_rules{
                        for arg in &mut rule.arg_list(){
                            let argument = config.__get_as_arg(arg, index);
                            rule.resolve_argument(arg, argument);
                        }
                    }
                }
            )
        }
        None => quote!(),
    };

    // 2. Generate RuleSet executable here
    Ok(quote!(
        // pub struct RuleSetExec <F: std::marker::Copy>
        // where F: FnOnce(HashMap<String, Argument>) -> Box<dyn Rulable>,
        pub struct RuleSetExec
        {
            name: String,
            unready_rules: HashMap<String, UnreadyRules>,
            rules: HashMap<String, ReadyRules>,
        }
        // impl <F: std::marker::Copy>RuleSetExec <F>
        // where F: FnOnce(HashMap<String, Argument>) -> Box<dyn Rulable>,
        impl RuleSetExec
        {
            pub fn init() -> Self{
                RuleSetExec{
                    name: #ruleset_name.to_string(),
                    unready_rules: HashMap::new(),
                    rules: HashMap::new(),
                }
            }
            // pub fn add_rule<F: std::marker::Copy>(&mut self, name: String, rule: F)
            pub fn add_unready_rule(&mut self, name: String, rule: UnreadyRules)
            {
                self.unready_rules.insert(name, rule);
            }

            pub fn check_arg_resolved(&mut self){
                for (rname, u_rule) in &self.unready_rules{
                    match u_rule.check_arg_resolved(){
                        Some(resolved_rule) => {
                            self.rules.insert(rname.to_string(), resolved_rule);
                        },
                        None => {

                        }
                    }
                }
            }

            #config

            pub fn gen_ruleset(&mut self, ruleset: &mut RuleSet<ActionClausesV2>, owner: u32){
                ruleset.update_name(&self.name);
                ruleset.update_owner_addr(Some(AddressKind::IntegerKind(owner)));
                for rname in vec![#(#rule_names),*]{
                    self.unready_rules
                    .get_mut(rname)
                    .expect("unable to find rule")
                    .gen_ruleset(ruleset);
                }
            }

            pub async fn execute(&self){}
        }
    ))
}

pub(super) fn generate_rule(
    rule_expr: &mut RuleExpr,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    // Get the basis information of Rule
    // rule rule_name <interface_1, interface_2> (arguments) {cond{} => act{}}

    // RuleName definition: rule_name
    let rule_name = &*rule_expr.name;
    let rule_name_string = String::from(&*rule_name.name);
    let rule_name_token = format_ident!("{}", &*rule_name.name);
    let unready_rule_name_token = format_ident!("unready_{}", &*rule_name.name);

    if ident_tracker.exist_rule_name(&*rule_name.name) {
        return Err(RuLaCompileError::RuleDuplicationError);
    } else {
        ident_tracker.add_rule_name(&rule_name.name);
    }

    // Interface Definition: <interface_1, interface_2>
    let mut interface_idents = vec![];
    for interface in &rule_expr.interface {
        // Check if the interface name exists
        if !ident_tracker.exist_interface(&*interface.name) {
            return Err(RuLaCompileError::NoInterfaceFoundError);
        }
        let gen_str = *interface.name.clone();
        interface_idents.push(quote!(#gen_str.to_string()));
    }

    // Rule Argument generation: (arg1, arg2, ...)
    let mut argument_adder = vec![];
    for arg in &mut rule_expr.args {
        ident_tracker.register(
            &*arg.name,
            Identifier::new(IdentType::RuleArgument, helper::type_filler(&arg)),
        );
        let arg_string = *arg.name.clone();
        argument_adder.push(quote!(
                let arg = Argument::init();
                empty_argument.insert(#arg_string.to_string(), arg);
        ))
    }

    // RuleContent inside the Rule def: {RuleContent}
    let generated = generate_rule_content(
        &mut rule_expr.rule_content,
        Some(&rule_name_string),
        ident_tracker,
    )
    .unwrap();

    // To generate static conditions and actions
    let static_cond = generate_static_cond(
        &mut *rule_expr.rule_content.condition_expr,
        Some(&*rule_name.name),
        ident_tracker,
    )
    .unwrap();
    let static_act = generate_static_act(
        &mut *rule_expr.rule_content.action_expr,
        Some(&*rule_name.name),
        ident_tracker,
    )
    .unwrap();

    Ok(quote!(
        pub struct #rule_name_token{
            interface_names: Vec<String>,
            arguments: HashMap<String, Argument>,
        }

        pub struct #unready_rule_name_token{
            interface_names: Vec<String>,
            // Should copy interface information laters
            static_interfaces: __StaticInterface,
            arguments: HashMap<String, Argument>,
        }

        impl #unready_rule_name_token{
            pub fn new(__static_interface:__StaticInterface) -> #unready_rule_name_token{
                // 1. prepare empty arguments based on arugment list
                let mut empty_argument = HashMap::new();
                #(#argument_adder);*;
                // 2. return structure
                #unready_rule_name_token{
                    interface_names: vec![#(#interface_idents),*],
                    static_interfaces: __static_interface,
                    arguments: empty_argument,
                }
            }

            pub fn argument_resolved(&self) -> Option<ReadyRules> {
                for (_, arg) in &self.arguments{
                    if !arg.resolved(){
                        return None;
                    }
                }
                Some(
                    ReadyRules::#rule_name_token(
                    #rule_name_token{
                        interface_names: self.interface_names.clone(),
                        arguments: self.arguments.clone(),
                    }))
            }

            pub fn arg_exist(&self, arg_name: &str)-> bool {
                self.arguments.contains_key(arg_name)
            }

            pub fn resolve_argument(&mut self, arg_name: &str, new_arg: Argument){
                let mut arg = self.arguments.get_mut(arg_name).expect("Unable to find argument");
                *arg = new_arg
            }

            pub fn arg_list(&self) -> Vec<String>{
                let mut arg_vec = vec![];
                for (arg, _) in &self.arguments{
                    arg_vec.push(arg.clone());
                }
                arg_vec
            }
            #[doc = "No execution, but gen ruleset"]
            fn static_ruleset_gen(&mut self) -> Stage<ActionClausesV2>{
                let mut stage = Stage::<ActionClausesV2>::new();
                #[doc = "Initialy, this starts just a single rule, but if there is match or if expression, this should be expanded."]
                let rules = Rc::new(RefCell::new(vec![RefCell::new(Rule::<ActionClausesV2>::new(#rule_name_string))]));
                #static_cond
                #static_act
                for rule in &*rules.borrow(){
                    stage.add_rule(rule.borrow().clone());
                }
                stage
            }

            fn gen_ruleset(&mut self, ruleset: &mut RuleSet<ActionClausesV2>){
                let stage = self.static_ruleset_gen();
                ruleset.add_stage(stage);
            }

        }
        impl #rule_name_token{
            #generated
        }

    ))
}

pub(super) fn generate_rule_content(
    rule_content_expr: &mut RuleContentExpr,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    // Actions to be performed
    let generated_act = generate_act(
        &mut *rule_content_expr.action_expr,
        rule_name,
        ident_tracker,
    )
    .unwrap();

    let generated_cond = generate_cond(
        &mut *rule_content_expr.condition_expr,
        rule_name,
        ident_tracker,
        &generated_act,
    )
    .unwrap();

    let post_process = &mut rule_content_expr.post_processing;
    let mut post_processes = vec![];
    for stmt in post_process {
        post_processes.push(generate_stmt(stmt, None, ident_tracker, false, false).unwrap());
    }
    // Generate executable
    // Here we should consider to use TaskMonitor in tokio to track the status of watched values
    Ok(quote!(
        async fn condition(&self) -> bool{
            let interface = INTERFACES.get().expect("Unable to get interface table").lock().await;
            #generated_cond
        }

        fn post_process(&self){
            #(#post_processes)*
        }
        // pub is implied in trait
        async fn execute(&self){
            loop{
                let __done = self.condition().await;
                if __done{
                    break;
                }
                sleep(Duration::from_micros(100));
            }
        }
    ))
}

pub(super) fn generate_watch(
    watch_expr: &mut WatchExpr,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
    in_static: bool,
) -> IResult<TokenStream> {
    // Watch expression defines the set of parameters whose state can be changed in the future
    // e.g.
    // watch{
    //      let qubit = qn0.get_qubit();
    // }
    let (in_rule, rname) = helper::check_in(rule_name);
    let watched_values = if in_rule {
        let mut generated_watch = vec![];
        for value in &mut watch_expr.watched_values {
            // start watch value generation process with `in_watch=true`
            generated_watch
                .push(generate_let(value, Some(&rname), ident_tracker, true, in_static).unwrap());
        }
        generated_watch
    } else {
        return Err(RuLaCompileError::NoRuleFoundError);
    };

    Ok(quote!(
        #(#watched_values)*
    ))
}

pub(super) fn generate_cond(
    cond_expr: &mut CondExpr,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
    act_tokens: &TokenStream,
) -> IResult<TokenStream> {
    let (in_rule, r_name) = helper::check_in(rule_name);

    if !in_rule {
        return Err(RuLaCompileError::NoRuleFoundError);
    }

    // Generate condition clauses
    let generated_clauses = {
        let mut clauses = vec![];
        for clause in &mut cond_expr.clauses {
            clauses.push(generate_awaitable(clause, Some(&r_name), ident_tracker, false).unwrap());
        }
        clauses
    };

    // If there is a watch expression, generate watch expression
    let generated_watch = match &mut *cond_expr.watch_expr {
        Some(watch_expr) => {
            generate_watch(watch_expr, Some(&r_name), ident_tracker, false).unwrap()
        }
        None => quote!(),
    };

    Ok(quote!(
        #generated_watch
        if #(#generated_clauses)&&*{
        // self.action().await;
        (||async {
            #act_tokens
        })().await;
        return true
        }else{
        return false
        };
    ))
}

pub(super) fn generate_awaitable(
    awaitable_expr: &mut Awaitable,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
    in_static: bool,
) -> IResult<TokenStream> {
    // Condition clauses should be awaited to be met
    // Check if the rule name is properly set
    let (in_rule, _) = helper::check_in(rule_name);
    if !in_rule {
        return Err(RuLaCompileError::NoRuleFoundError);
    }

    let do_await = if in_static { false } else { true };
    let awaitable = match awaitable_expr {
        Awaitable::FnCall(fn_call) => {
            // This should be flex
            let generated_fn_call = generate_fn_call(
                fn_call,
                rule_name,
                ident_tracker,
                do_await,
                in_static,
                false,
            )
            .unwrap();
            quote!(
                #generated_fn_call
            )
        }
        Awaitable::VariableCallExpr(val_call) => {
            // Check if the value is properly watched or not
            let generated_val = generate_variable_call(
                val_call,
                rule_name,
                ident_tracker,
                do_await,
                in_static,
                false,
            )
            .unwrap();
            quote!(#generated_val)
        }
        Awaitable::Comp(comp) => {
            let generated_comp = if in_static {
                generate_comp(comp, rule_name, ident_tracker, false, in_static).unwrap()
            } else {
                generate_comp(comp, rule_name, ident_tracker, true, in_static).unwrap()
            };
            quote!(#generated_comp)
        }
    };
    if in_static {
        Ok(quote!(
            let _ = #awaitable;
        ))
    } else {
        Ok(quote!(
            #awaitable
        ))
    }
}

pub(super) fn generate_act(
    act_expr: &mut ActExpr,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let mut action_calls = vec![];

    for action in &mut act_expr.operatable {
        // TODO: Should integrate but for now, we need to know if there is ";" at the end
        match &mut *action.kind {
            StmtKind::Let(let_stmt) => action_calls
                .push(generate_let(let_stmt, rule_name, ident_tracker, false, false).unwrap()),
            _ => {
                let generated =
                    generate_stmt(action, rule_name, ident_tracker, true, false).unwrap();
                action_calls.push(quote!(
                    #generated;
                ))
            }
        }
    }
    Ok(quote!(
        #(#action_calls)*
    ))
}

pub(super) fn generate_static_act(
    action_expr: &mut ActExpr,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let mut static_action_calls = vec![];

    for action in &mut *action_expr.operatable {
        match &mut *action.kind {
            StmtKind::Let(let_stmt) => static_action_calls
                .push(generate_let(let_stmt, rule_name, ident_tracker, false, true).unwrap()),
            _ => {
                let generated =
                    generate_stmt(action, rule_name, ident_tracker, false, true).unwrap();
                static_action_calls.push(quote!(
                    #generated;
                ))
            }
        }
    }
    Ok(quote!(
        #(#static_action_calls);*
    ))
}

pub(super) fn generate_static_cond(
    cond_expr: &mut CondExpr,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let mut generated_clauses = vec![];
    for clause in &mut cond_expr.clauses {
        generated_clauses.push(generate_awaitable(clause, rule_name, ident_tracker, true).unwrap());
    }
    match &mut *cond_expr.watch_expr {
        Some(watch_expr) => {
            let generated_watch =
                generate_watch(watch_expr, rule_name, ident_tracker, true).unwrap();
            Ok(quote!(
               #generated_watch
               #(#generated_clauses)*
            ))
        }
        None => Ok(quote!(
            #(#generated_clauses)*
        )),
    }
}

pub(super) fn generate_variable_call(
    variable_call_expr: &mut VariableCallExpr,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
    do_await: bool,
    in_static: bool,
    in_closure: bool,
) -> IResult<TokenStream> {
    let mut variable_calls = vec![];

    if variable_call_expr.variables.len() == 0 {
        panic!("Internal Error. This needs to be treated in parser.")
    }
    for (index, val) in &mut variable_call_expr.variables.iter().enumerate() {
        let specified = if index == 0 {
            false
        } else if do_await {
            true
        } else {
            match &variable_call_expr.variables[index - 1] {
                Callable::Ident(ident) => {
                    // If the variable is registered as qubit, operations should be awaited
                    match ident_tracker.check_ident_type(&ident.name) {
                        IdentType::QubitInterface => true,
                        IdentType::QnicInterface => true,
                        _ => false,
                    }
                }
                _ => false,
            }
        };
        match val.clone() {
            Callable::FnCall(mut fn_call_inner) => {
                if specified {
                    variable_calls.push(
                        generate_fn_call(
                            &mut fn_call_inner,
                            rule_name,
                            ident_tracker,
                            true,
                            in_static,
                            in_closure,
                        )
                        .unwrap(),
                    );
                } else {
                    variable_calls.push(
                        generate_fn_call(
                            &mut fn_call_inner,
                            rule_name,
                            ident_tracker,
                            false,
                            in_static,
                            in_closure,
                        )
                        .unwrap(),
                    );
                }
            }
            Callable::Ident(ident) => {
                variable_calls
                    .push(generate_ident(&ident, ident_tracker, false, in_static).unwrap());
            }
        }
    }

    Ok(quote!(#(#variable_calls).*))
}

pub(super) fn generate_array(
    array_expr: &Array,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let mut items = vec![];
    for lits in array_expr.items.iter() {
        items.push(generate_lit(lits, ident_tracker, false, false).unwrap());
    }
    Ok(quote!(vec![#(#items),*]))
}

pub(super) fn generate_lit(
    lit: &Lit,
    ident_tracker: &mut IdentTracker,
    in_static: bool,
    in_match: bool,
) -> IResult<TokenStream> {
    match &*lit.kind {
        LitKind::Ident(ident_lit) => {
            Ok(generate_ident(ident_lit, ident_tracker, false, in_static).unwrap())
        }
        LitKind::NumberLit(number_lit) => {
            let val = LitFloat::new(&*number_lit.value.name, Span::call_site());
            Ok(quote!(#val))
        }
        LitKind::StringLit(string_lit) => {
            let val = SynLit::Str(LitStr::new(&string_lit.as_string(), Span::call_site()));
            if in_match {
                Ok(quote!(#val))
            } else {
                Ok(quote!(#val.to_string()))
            }
        }
        _ => todo!("{:#?} not yet implemented", lit),
    }
}

pub(super) fn generate_term(term_expr: f64) -> IResult<TokenStream> {
    // We could make Term struct instead of direct calc
    // For now, this function just returns calc result as f64
    let val = LitFloat::new(&term_expr.to_string(), Span::call_site());
    Ok(quote!(#val))
}
// Generate identifier token from Ident ast
pub(super) fn generate_ident(
    ident: &Ident,
    ident_tracker: &mut IdentTracker,
    with_type_annotation: bool,
    in_static: bool,
) -> IResult<TokenStream> {
    // If the identifier is not properly set, return error
    if &*ident == &Ident::place_holder() {
        return Err(RuLaCompileError::FailedToSetValueError);
    }

    let identifier = format_ident!("{}", *ident.name);
    let ident_contents = match ident_tracker.check_ident_type(&*ident.name) {
        IdentType::QnicInterface => {
            let ident_str = SynLit::Str(LitStr::new(&*ident.name, Span::call_site()));
            if !in_static {
                quote!(
                    interface.get(#ident_str)
                    .expect("unable to get interface")
                )
            } else {
                // In static
                quote!(
                    self.static_interfaces.__get_interface(#ident_str)
                )
            }
        }
        IdentType::WatchedValue => quote!(#identifier),
        IdentType::RuleArgument => {
            let ident_str = SynLit::Str(LitStr::new(&*ident.name, Span::call_site()));
            quote!(
                self.arguments.get(#ident_str).unwrap()
            )
        }
        _ => {
            quote!(#identifier)
        }
    };
    if with_type_annotation {
        match &*ident.type_hint {
            Some(hint) => {
                let type_hint = generate_type_hint(hint).unwrap();
                return Ok(quote!(#ident_contents:#type_hint));
            }
            None => return Err(RuLaCompileError::NoTypeFoundError),
        }
    } else {
        match ident_tracker.check_type_hint(&*ident.name) {
            TypeHint::Integer64 => Ok(quote!(#ident_contents.eval_int64())),
            TypeHint::UnsignedInteger64 => Ok(quote!(#ident_contents.eval_unsigned_int64())),
            TypeHint::Float64 => Ok(quote!(#ident_contents.eval_float64())),
            TypeHint::Str => Ok(quote!(#ident_contents.eval_str())),
            TypeHint::Boolean => Ok(quote!(#ident_contents.eval_bool())),
            TypeHint::Qubit => Ok(quote!(#ident_contents)),
            TypeHint::StrVector => Ok(quote!(#ident_contents.eval_str_vec())),
            TypeHint::I64Vector => Ok(quote!(#ident_contents.eval_i64_vec())),
            TypeHint::F64Vector => Ok(quote!(#ident_contents.eval_f64_vec())),
            TypeHint::U64Vector => Ok(quote!(#ident_contents.eval_u64_vec())),
            TypeHint::BoolVector => Ok(quote!(#ident_contents.eval_bool_vec())),
            TypeHint::Unknown => Ok(quote!(#ident_contents)),
            _ => {
                todo!("{:#?}", ident)
            }
        }
    }
}

pub(super) fn generate_type_hint(type_hint: &TypeDef) -> IResult<TokenStream> {
    match type_hint {
        TypeDef::Boolean => Ok(quote!(bool)),
        TypeDef::Integer => Ok(quote!(i64)),
        TypeDef::UnsignedInteger => Ok(quote!(u64)),
        TypeDef::Float => Ok(quote!(f64)),
        TypeDef::Str => Ok(quote!(String)),
        TypeDef::Qubit => Ok(quote!(QubitInterface)),
        TypeDef::Vector(inner) => {
            let inner_type = generate_type_hint(inner).unwrap();
            Ok(quote!(Vec<#inner_type>))
        }
        TypeDef::PlaceHolder => {
            return Err(RuLaCompileError::RuLaInitializationError(
                InitializationError::new("at type hint generation"),
            ))
        }
        _ => todo!("type {:#?} not yet implemented", type_hint),
    }
}

pub mod helper {
    use super::*;
    use proc_macro2::TokenStream;

    pub fn check_in(named: Option<&String>) -> (bool, String) {
        match named {
            Some(name) => (true, name.clone()),
            None => (false, String::from("")),
        }
    }

    pub fn generate_test() -> TokenStream {
        quote!(
            #[doc = "This is generated for entanglement_swapping.rula"]
            #[tokio::test]
            async fn test_interface() {
                assert!(INTERFACES.get().is_none());
                rula::initialize_interface().await;
                let interface = INTERFACES.get().expect("Failed to get interface table");
                assert!(interface.lock().await.contains_key("qn0"));
                assert!(interface.lock().await.contains_key("qn1"));
            }

            #[tokio::test]
            async fn run_main() {
                main().await;
                assert_eq!(1, 2);
            }
        )
    }

    pub fn type_filler(ident: &Ident) -> TypeHint {
        match &*ident.type_hint {
            Some(hint) => {
                match hint {
                    // Convert to available type
                    TypeDef::Integer => TypeHint::Integer64,
                    TypeDef::Float => TypeHint::Float64,
                    TypeDef::Str => TypeHint::Str,
                    TypeDef::UnsignedInteger => TypeHint::UnsignedInteger64,
                    TypeDef::Boolean => TypeHint::Boolean,
                    TypeDef::Vector(inner) => match &**inner {
                        TypeDef::Integer => TypeHint::I64Vector,
                        TypeDef::Float => TypeHint::F64Vector,
                        TypeDef::Str => TypeHint::StrVector,
                        TypeDef::UnsignedInteger => TypeHint::U64Vector,
                        TypeDef::Boolean => TypeHint::BoolVector,
                        _ => todo!(),
                    },
                    _ => {
                        todo!("Other type conversion is not yet implemented {:#?}", hint)
                    }
                }
            }
            None => TypeHint::Unknown,
        }
    }

    pub fn arg_val_type_token_gen(ident: &Ident) -> (TokenStream, TokenStream) {
        match &*ident.type_hint {
            Some(hint) => {
                match hint {
                    // Convert to available type
                    TypeDef::Integer => (quote!(Integer64), quote!(LibTypeHint::Integer64)),
                    TypeDef::Float => (quote!(Float64), quote!(LibTypeHint::Float64)),
                    TypeDef::Str => (quote!(Str), quote!(LibTypeHint::Str)),
                    TypeDef::UnsignedInteger => (
                        quote!(UnsignedInteger64),
                        quote!(LibTypeHint::UnsignedInteger64),
                    ),
                    TypeDef::Boolean => (quote!(Boolean), quote!(TypeHint::Boolean)),
                    TypeDef::Vector(content_type) => match &**content_type {
                        TypeDef::Integer => (
                            quote!(I64Vector),
                            quote!(LibTypeHint::Vector(Box::new(LibTypeHint::Integer64))),
                        ),
                        TypeDef::UnsignedInteger => (
                            quote!(U64Vector),
                            quote!(LibTypeHint::Vector(Box::new(
                                LibTypeHint::UnsignedInteger64
                            ))),
                        ),
                        TypeDef::Float => (
                            quote!(F64Vector),
                            quote!(LibTypeHint::Vector(Box::new(LibTypeHint::Float64))),
                        ),
                        TypeDef::Str => (
                            quote!(StrVector),
                            quote!(LibTypeHint::Vector(Box::new(LibTypeHint::Str))),
                        ),
                        TypeDef::Boolean => (
                            quote!(BoolVector),
                            quote!(LibTypeHint::Vector(Box::new(LibTypeHint::Boolean))),
                        ),
                        _ => todo!(),
                    },
                    _ => {
                        todo!("Other type conversion is not yet implemented {:#?}", hint)
                    }
                }
            }
            None => (quote!(), quote!(TypeHint::Unknown)),
        }
    }
}
