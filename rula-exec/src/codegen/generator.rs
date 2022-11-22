// This is entory point to generate code from AST
use super::default_token::*;
use super::error::*;
use super::identifier::*;
use super::rule_meta::*;
use super::ruleset_generator::RuleSetFactory;
use super::IResult;

use crate::rulep::action::v2::ActionClauses;
use crate::rulep::action::Action;
use crate::rulep::condition::Condition;
use crate::rulep::ruleset::Rule;
use crate::wrapper::qnic_wrapper::QnicInterfaceWrapper;
use rula_parser::parser::ast::*;

use once_cell::sync::OnceCell;
use proc_macro2::{Span, TokenStream};
use std::path::PathBuf;
use std::sync::Mutex;
use syn::Lit as SynLit;
use syn::{LitFloat, LitInt, LitStr};

// These are used but only in quote! macro
#[allow(unused_imports)]
use mock_components::hardware::result::MeasResult;
#[allow(unused_imports)]
use std::net::SocketAddr;

// Right now, compilation is all single thread. In the future, this could be multi thread.
// Mutex would be a good choice for that.
static RULES: OnceCell<Mutex<Vec<String>>> = OnceCell::new();

static RULESET_FACTORY: OnceCell<Mutex<RuleSetFactory>> = OnceCell::new();

/// Generate corresponding rust code from ast
/// Every nested generators returns a piece of TokenStream
pub fn generate(
    ast_tree: &mut Vec<AstNode>,
    with_ruleset: bool,
    config_path: Option<PathBuf>,
) -> IResult<TokenStream> {
    // initialize all the global values (RULESET, RULE_TABLE)
    initialize_singleton();

    // ident_tracker tracks all the identifier especially special values
    let mut ident_tracker = IdentTracker::new();

    let rules = RULES.get().expect("Unable to get rule table");

    // RuLa starts here
    if ast_tree.len() > 1 {
        todo!("This should get a root AST node. Fix it");
    }

    // Generated rula program
    let rula_program = match &mut ast_tree[0] {
        AstNode::RuLa(rula) => generate_rula(rula, &mut ident_tracker).unwrap(),
        AstNode::PlaceHolder => {
            return Err(RuLaCompileError::RuLaInitializationError(
                InitializationError::new("at very first generation"),
            ))
        }
    };

    // Generate tests for generaetd rust code.
    // This is just returns tokenstream that describes the test contents
    let generated_tests = helper::generate_test();

    // Used to make enums for the different types of rules
    let mut rule_names = vec![];
    let mut unready_rule_names = vec![];
    let mut ready_rule_names = vec![];
    let mut unready_rule_checker = vec![];
    let mut arg_list_generator = vec![];
    let mut arg_resolvers = vec![];
    let mut ruleset_gen_enum = vec![];
    for rname in rules.lock().unwrap().iter() {
        let rname_unready = format_ident!("unready_{}", rname);
        unready_rule_names.push(quote!(
            #rname_unready(#rname_unready)
        ));

        let rname_ready = format_ident!("{}", rname);
        ready_rule_names.push(quote!(
            #rname_ready(#rname_ready)
        ));

        rule_names.push(quote!(
            ruleset.add_unready_rule(
                String::from(#rname),
                rula::UnreadyRules::#rname_unready(rula::#rname_unready::new().await)
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

    let config_gen = match config_path {
        Some(path) => {
            let config_name = match &ident_tracker.config_name {
                Some(conf_name) => {
                    format_ident!("{}", conf_name)
                }
                None => panic!("Failed to resolve config name"),
            };
            let path_lit = &path.into_os_string().into_string().unwrap();
            quote!(
                let content = fs::read_to_string(#path_lit).unwrap();
                let mut config: rula::#config_name = toml::from_str(&content).unwrap();
                config.__finalize();
            )
        }
        None => {
            quote!()
        }
    };

    let ruleset_gen = if with_ruleset {
        quote!(
            let mut static_ruleset = RuleSet::<ActionClauses>::new("");
            ruleset.gen_ruleset(&mut static_ruleset);
            println!("{:#?}", &static_ruleset);
        )
    } else {
        quote!()
    };

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

    let default_imports = default_imports();

    // All ast are supposed to be evaluated here
    let rula_token_stream = quote!(
        use rula_lib as rula_std;
        use std::fs;
        use crate::rula_std::ruleset::ruleset::*;
        use rula_std::ruleset::action::v2::ActionClauses;
        #[allow(unused)]
        mod rula{
            #default_imports
            pub static INTERFACES: OnceCell<TokioMutex<HashMap<String, QnicInterface>>> = OnceCell::new();
            pub static STATIC_INTERFACES: OnceCell<StdMutex<HashMap<String, QnicInterface>>> = OnceCell::new();
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
        pub async fn main(){
            rula::initialize_interface().await;
            rula::initialize_static_interface();
            let mut rulesets = vec![];
            #config_gen
            for i in 0..#num_node{
                let mut ruleset = rula::RuleSetExec::init();
                #(#rule_names);*;
                ruleset.resolve_config(Box::new(&config), Some(i as usize));
                #ruleset_gen
                rulesets.push(static_ruleset);
            }
        }

        #[cfg(test)]
        mod tests{
            use super::*;
            use super::rula::*;
            #generated_tests
        }
    );
    Ok(rula_token_stream)
}

// Initialize singleton values
// RULESET_FACTORY: collect information needed to create a RuleSet
// RULESET: ruleset instance (will be deprecated in generator)
pub(super) fn initialize_singleton() {
    // Set the initial RuleSet so that other RuleSet call can only get without init
    assert!(RULESET_FACTORY.get().is_none());
    assert!(RULES.get().is_none());

    let initialize_ruleset_factory = || Mutex::new(RuleSetFactory::init());
    let _ = RULESET_FACTORY.get_or_init(initialize_ruleset_factory);

    let initialize_rule = || Mutex::new(vec![]);
    let _ = RULES.get_or_init(initialize_rule);
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
                stmts.push(generate_stmt(stmt, None, ident_tracker, false).unwrap())
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
    in_static: bool,
) -> IResult<TokenStream> {
    let generated_stmt = match &mut *stmt.kind {
        StmtKind::Let(let_stmt) => Ok(generate_let(
            let_stmt,
            rule_name,
            false,
            ident_tracker,
            in_static,
            &mut vec![],
        )
        .unwrap()),
        StmtKind::Interface(interface) => Ok(generate_interface(interface, ident_tracker).unwrap()),
        StmtKind::Config(config) => Ok(generate_config(config, ident_tracker).unwrap()),
        StmtKind::Expr(expr) => {
            Ok(generate_expr(expr, rule_name, ident_tracker, in_static).unwrap())
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
    in_watch: bool,
    ident_tracker: &mut IdentTracker,
    in_static: bool,
    action_var_collection: &mut Vec<TokenStream>,
) -> IResult<TokenStream> {
    // RuleSet singleton to store ruleset information
    let ruleset_factory = RULESET_FACTORY
        .get()
        .expect("Failed to get Ruleset factory");
    let (mut identifier, mut expr) = (quote!(), quote!());
    // If this let stmt is in the watch clause
    if in_watch {
        // Register values to watched_values in RuleMeta
        match rule_name {
            Some(name) => {
                // Don't wanna clone the expression here
                ruleset_factory.lock().unwrap().add_watch_value(
                    name,
                    &*let_stmt.ident.name,
                    Watchable::UnSet,
                );
                ident_tracker.register(
                    &*let_stmt.ident.name,
                    Identifier::new(IdentType::WatchedValue, TypeHint::Unknown),
                );
                match &*let_stmt.ident.type_hint {
                    Some(hint) => {
                        match hint {
                            TypeDef::Qubit => ident_tracker.register(
                                &*let_stmt.ident.name,
                                Identifier::new(IdentType::QubitInterface, TypeHint::Qubit),
                            ),
                            _ => {}
                        }
                        identifier =
                            generate_ident(&*let_stmt.ident, ident_tracker, true, in_static)
                                .unwrap();
                    }
                    None => {
                        identifier =
                            generate_ident(&*let_stmt.ident, ident_tracker, false, in_static)
                                .unwrap();
                    }
                }
                expr = generate_expr(&mut *let_stmt.expr, rule_name, ident_tracker, in_static)
                    .unwrap();
            }
            None => {
                todo!("No rule found")
            }
        }
    } else {
        match &*let_stmt.ident.type_hint {
            Some(hint) => {
                match hint {
                    TypeDef::Qubit => ident_tracker.register(
                        &*let_stmt.ident.name,
                        Identifier::new(IdentType::QubitInterface, TypeHint::Qubit),
                    ),
                    _ => {}
                }
                identifier =
                    generate_ident(&*let_stmt.ident, ident_tracker, true, in_static).unwrap();
            }
            None => {
                identifier =
                    generate_ident(&*let_stmt.ident, ident_tracker, false, in_static).unwrap();
            }
        }
        expr = generate_expr(&mut *let_stmt.expr, rule_name, ident_tracker, in_static).unwrap();
    }

    let mut interface_name_list = vec![];
    for interface_name in &ident_tracker.interface_names {
        // let string_name = SynLit::Str(LitStr::new(interface_name, Span::call_site()));
        interface_name_list.push(interface_name);
    }
    if in_watch {
        if in_static {
            let static_identifier = format_ident!("{}", &*let_stmt.ident.name);
            let clause_identifier = format_ident!("condition_clause_{}", &*let_stmt.ident.name);
            action_var_collection.push(quote!(#static_identifier));
            Ok(quote!(
                    let #static_identifier = #expr;
                    // match #clause_identifier{
                    //     Some(clause) => {
                    //         self.condition_clauses.push(clause);
                    //     },
                    //     None => {

                    //     },
                    // }
            ))
        } else {
            Ok(quote!(
                let mut #identifier = #expr.await;
            ))
        }
    } else {
        if in_static {
            let static_identifier = format_ident!("{}", &*let_stmt.ident.name);
            action_var_collection.push(quote!(#static_identifier));
            Ok(quote!(
                let mut #static_identifier = #expr.clone();
            ))
        } else {
            Ok(quote!(
                let mut #identifier = #expr;
            ))
        }
    }
}

pub(super) fn generate_interface(
    interface_expr: &mut Interface,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let mut interface_names = vec![];
    let ruleset_factory = RULESET_FACTORY
        .get()
        .expect("Failed to get RuleSet factory");

    for i in &mut interface_expr.interface {
        ruleset_factory
            .lock()
            .unwrap()
            .add_global_interface(&i.name, QnicInterfaceWrapper::place_holder());
        // TODO: clean up
        ident_tracker.register(
            &i.name,
            Identifier::new(IdentType::QnicInterface, TypeHint::Unknown),
        );
        i.update_ident_type(IdentType::QnicInterface);
        interface_names.push(SynLit::Str(LitStr::new(&i.name, Span::call_site())));
    }
    let interface_group_name = match &mut *interface_expr.group_name {
        Some(group_name) => {
            group_name.update_ident_type(IdentType::QnicInterface);
            &*group_name.name
        }
        None => "",
    };
    interface_names.push(SynLit::Str(LitStr::new(
        interface_group_name,
        Span::call_site(),
    )));
    ident_tracker.register(
        interface_group_name,
        Identifier::new(IdentType::QnicInterface, TypeHint::Unknown),
    );
    Ok(quote!(
        pub async fn initialize_interface() {
            assert!(INTERFACES.get().is_none());
            let initialize_interface = || TokioMutex::new(HashMap::new());
            INTERFACES.get_or_init(initialize_interface);
            let interface_list = INTERFACES.get().expect("Failed to get interface");
            for interface_name in vec![#(#interface_names),*] {
                let mock_qnic = QnicInterface::generate_mock_interface(interface_name, 10);
                interface_list
                    .lock()
                    .await
                    .insert(interface_name.to_string(), mock_qnic);
            }
        }

        pub fn initialize_static_interface() {
            assert!(STATIC_INTERFACES.get().is_none());
            let initialize_interface = || StdMutex::new(HashMap::new());
            STATIC_INTERFACES.get_or_init(initialize_interface);
            let interface_list = STATIC_INTERFACES.get().expect("Failed to get interface");
            for interface_name in vec![#(#interface_names),*] {
                let mock_qnic = QnicInterface::generate_mock_interface(interface_name, 10);
                interface_list
                    .lock()
                    .unwrap()
                    .insert(interface_name.to_string(), mock_qnic);
            }
        }
    ))
}

pub(super) fn generate_expr(
    expr: &mut Expr,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
    in_static: bool,
) -> IResult<TokenStream> {
    match &mut *expr.kind {
        ExprKind::Import(import_expr) => Ok(generate_import(import_expr, ident_tracker).unwrap()),
        ExprKind::If(if_expr) => Ok(generate_if(if_expr, ident_tracker).unwrap()),
        ExprKind::For(for_expr) => Ok(generate_for(for_expr, ident_tracker).unwrap()),
        ExprKind::While(while_expr) => Ok(generate_while(while_expr, ident_tracker).unwrap()),
        ExprKind::FnDef(fn_def_expr) => Ok(generate_fn_def(fn_def_expr, ident_tracker).unwrap()),
        ExprKind::FnCall(fn_call_expr) => {
            Ok(generate_fn_call(fn_call_expr, rule_name, ident_tracker, false, in_static).unwrap())
        }
        ExprKind::Struct(struct_expr) => Ok(generate_struct(struct_expr, ident_tracker).unwrap()),
        ExprKind::Return(return_expr) => Ok(generate_return(return_expr, ident_tracker).unwrap()),
        ExprKind::Match(match_expr) => {
            Ok(generate_match(match_expr, ident_tracker, in_static).unwrap())
        }
        ExprKind::Comp(comp_expr) => {
            Ok(generate_comp(comp_expr, rule_name, ident_tracker).unwrap())
        }
        ExprKind::RuleSetExpr(ruleset_expr) => {
            Ok(generate_ruleset_expr(ruleset_expr, ident_tracker).unwrap())
        }
        ExprKind::RuleExpr(rule_expr) => Ok(generate_rule(rule_expr, ident_tracker).unwrap()),
        ExprKind::CondExpr(cond_expr) => {
            Ok(generate_cond(cond_expr, rule_name, ident_tracker, &quote!()).unwrap())
        }
        ExprKind::ActExpr(act_expr) => {
            Ok(generate_act(act_expr, rule_name, ident_tracker).unwrap())
        }
        ExprKind::VariableCallExpr(variable_call_expr) => {
            Ok(
                generate_variable_call(variable_call_expr, rule_name, ident_tracker, in_static)
                    .unwrap(),
            )
        }
        ExprKind::Array(array_expr) => Ok(generate_array(&array_expr, ident_tracker).unwrap()),
        ExprKind::Lit(lit_expr) => Ok(generate_lit(&lit_expr, ident_tracker, in_static).unwrap()),
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
) -> IResult<TokenStream> {
    // block could have invalid expression here.
    let block_quote = generate_expr(&mut *if_expr.block, None, ident_tracker, false).unwrap();
    let stmt_quote = generate_stmt(&mut *if_expr.stmt, None, ident_tracker, false).unwrap();
    if if_expr.elif.len() > 0 {
        // no elif statement
        if if_expr.elif[0] == None {
            match &mut *if_expr.els {
                // With else statement
                Some(els_stmt) => {
                    let els_stmt_quote =
                        generate_stmt(els_stmt, None, ident_tracker, false).unwrap();
                    return Ok(quote!(
                        if #block_quote{
                            #stmt_quote
                        }else{
                            #els_stmt_quote
                        }
                    ));
                }
                None => {
                    // No error statement
                    // This could be error in rust code
                    return Ok(quote!(
                        if #block_quote{
                            #stmt_quote
                        }
                    ));
                }
            }
        } else {
            // No elif expr
            let mut elif_quotes = vec![];
            for elif_expr in &mut *if_expr.elif {
                match elif_expr {
                    Some(if_expr) => elif_quotes.push(generate_if(if_expr, ident_tracker).unwrap()),
                    None => {
                        unreachable!()
                    }
                }
            }

            match &mut *if_expr.els {
                Some(els_stmt) => {
                    let els_stmt_quote =
                        generate_stmt(els_stmt, None, ident_tracker, false).unwrap();
                    Ok(quote!(
                        if #block_quote {
                            #stmt_quote
                        }#(else #elif_quotes)*

                        else{
                            #els_stmt_quote
                        }
                    ))
                }
                None => Ok(quote!(
                    if #block_quote {
                        #stmt_quote
                    }#(elif #elif_quotes)*
                )),
            }
        }
    } else {
        // Duplication?
        panic!("This should not happen in the current initialization");
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
    let generator = generate_expr(&mut for_expr.generator, None, ident_tracker, false).unwrap();
    let stmt = generate_stmt(&mut for_expr.stmt, None, ident_tracker, false).unwrap();
    if ident_list.len() == 1 {
        let var = &ident_list[0];
        Ok(quote!(
            for #var in #generator {
                #stmt
            }
        ))
    } else if ident_list.len() > 1 {
        Ok(quote!(
            for (#(#ident_list),* ) in generator {
                #stmt
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
    let block = generate_expr(&mut while_expr.block, None, ident_tracker, false).unwrap();
    let stmt = generate_stmt(&mut while_expr.stmt, None, ident_tracker, false).unwrap();
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
    let stmt = generate_stmt(&mut fn_def_expr.stmt, None, ident_tracker, false).unwrap();
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
            let generated_arg = generate_expr(arg, rule_name, ident_tracker, in_static).unwrap();
            if in_static {
                args.push(generated_arg);
            } else {
                args.push(quote!(&#generated_arg));
            }
        }
        args
    };
    if in_static {
        Ok(quote!(
            #fn_name(Rc::clone(&condition_clauses), Rc::clone(&action_clauses), #(#generated_arguments.clone()),*)
        ))
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

pub(super) fn generate_return(
    return_expr: &mut Return,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let expr = generate_expr(&mut return_expr.target, None, ident_tracker, false).unwrap();
    Ok(quote!(
        return #expr;
    ))
}

pub(super) fn generate_match(
    match_expr: &mut Match,
    ident_tracker: &mut IdentTracker,
    in_static: bool,
) -> IResult<TokenStream> {
    let generated_expr =
        generate_expr(&mut match_expr.expr, None, ident_tracker, in_static).unwrap();
    let mut match_arms = vec![];

    for arm in &mut match_expr.match_arms {
        match_arms.push(generate_match_arm(arm, ident_tracker, in_static).unwrap());
    }

    let finally = match &mut *match_expr.finally {
        Some(fin) => generate_match_action(fin, ident_tracker, in_static).unwrap(),
        None => quote!(),
    };

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
}

pub(super) fn generate_match_arm(
    match_arm: &mut MatchArm,
    ident_tracker: &mut IdentTracker,
    in_static: bool,
) -> IResult<TokenStream> {
    let generated_match_condition =
        generate_match_condition(&mut *match_arm.condition, ident_tracker).unwrap();
    let generated_match_action =
        generate_match_action(&mut *match_arm.action, ident_tracker, in_static).unwrap();
    Ok(quote!(#generated_match_condition => {#generated_match_action}))
}

pub(super) fn generate_match_condition(
    match_condition: &mut MatchCondition,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    // Right now, satisfiable can only take literals, but in the future, this should be more flexible
    match &mut *match_condition.satisfiable {
        Satisfiable::Lit(literal) => Ok(generate_lit(literal, ident_tracker, false).unwrap()),
        _ => Err(RuLaCompileError::RuLaInitializationError(
            InitializationError::new("at match condition"),
        )),
    }
}

pub(super) fn generate_match_action(
    match_action: &mut MatchAction,
    ident_tracker: &mut IdentTracker,
    in_static: bool,
) -> IResult<TokenStream> {
    let mut actionables = vec![];
    for actionable in &mut *match_action.actionable {
        actionables.push(generate_expr(actionable, None, ident_tracker, in_static).unwrap());
    }
    Ok(quote!(
        #(#actionables);*
    ))
}

pub(super) fn generate_comp(
    comp_expr: &mut Comp,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    let lhs = generate_expr(&mut comp_expr.lhs, rule_name, ident_tracker, false).unwrap();
    let rhs = generate_expr(&mut comp_expr.rhs, rule_name, ident_tracker, false).unwrap();
    let op = match *comp_expr.comp_op {
        CompOpKind::Lt => quote!(<),
        CompOpKind::Gt => quote!(>),
        CompOpKind::LtE => quote!(<=),
        CompOpKind::GtE => quote!(>=),
        CompOpKind::Eq => quote!(==),
        CompOpKind::Nq => quote!(!=),
        CompOpKind::PlaceHolder => {
            return Err(RuLaCompileError::RuLaInitializationError(
                InitializationError::new("at generating comp expression"),
            ))
        }
    };
    Ok(quote!(#lhs #op #rhs))
}

pub(super) fn generate_ruleset_expr(
    ruleset_expr: &RuleSetExpr,
    ident_tracker: &mut IdentTracker,
) -> IResult<TokenStream> {
    // 1. Generate static RuleSet for serialized output
    // Get meta information for this RuleSet
    let ruleset_name = &*ruleset_expr.name.name;

    let rules = RULES.get().expect("Failed to get Rule table");

    let mut rule_names = vec![];
    // Closure that gets rule_name and evaluate if the rule is inside the table or not
    // If that Rule is in the table, add it to RuleSet
    for rule in &ruleset_expr.rules {
        match rule {
            // Ordinary Rule call (e.g. swapping_rule())
            RuleIdentifier::FnCall(fn_call_expr) => {
                // Rule without any return values
                rules
                    .lock()
                    .unwrap()
                    .push(String::from(&*fn_call_expr.func_name.name));
                rule_names.push(String::from(&*fn_call_expr.func_name.name));
                // get argument name
                // let rule_argument = &fn_call_expr.arguments;
            }
            RuleIdentifier::Let(let_stmt) => {
                // Rule with return value
                // In the static RuleSet, it doesn't care if there is a return value or not
                // The name of the function must be in RULE_TABLE
                let expr = &*let_stmt.expr;
                match &*expr.kind {
                    ExprKind::FnCall(fn_call) => {
                        // rule_add_evaluater(&*fn_call.func_name.name).unwrap();
                        rules
                            .lock()
                            .unwrap()
                            .push(String::from(&*fn_call.func_name.name));
                        rule_names.push(String::from(&*fn_call.func_name.name));
                    }
                    _ => unreachable!("So far RuleIdentifier can only be FnCall"),
                }
            }
        }
    }

    // Replace all the TBD information with configed values
    // Expand Config here
    let (conf_def, config) = match &*ruleset_expr.config {
        Some(conf_name) => {
            if Some(String::from(&*conf_name.name)) != ident_tracker.config_name {
                return Err(RuLaCompileError::NoConfigFound);
            }
            let name = generate_ident(conf_name, ident_tracker, false, false).unwrap();
            (
                quote!(config: #name),
                quote!(
                    pub fn resolve_config(&mut self, config: Box<&#name>, index: Option<usize>){
                        for (_, rule) in &mut self.unready_rules{
                            for arg in &mut rule.arg_list(){
                                let argument = config.__get_as_arg(arg, index);
                                rule.resolve_argument(arg, argument);
                            }
                        }
                    }
                ),
            )
        }
        None => (quote!(), quote!()),
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
            rule_arguments: Vec<HashMap<String, Argument>>
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
                    rule_arguments: vec![],
                    // #conf_def,
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

            pub fn gen_ruleset(&mut self, ruleset: &mut RuleSet<ActionClausesV2>){
                ruleset.update_name(&self.name);
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
    // 1. Generate Rules and store them to the table

    let ruleset_factory = RULESET_FACTORY
        .get()
        .expect("Failed to get ruleset facotyr");

    // Get the basis information of Rule
    let rule_name = &*rule_expr.name;
    let rule_name_string = String::from(&*rule_name.name);

    if ruleset_factory.lock().unwrap().exist_rule(&*rule_name.name) {
        return Err(RuLaCompileError::RuleDuplicationError);
    }

    // Prepare an empty Rule
    let mut rule = Rule::<ActionClauses>::new(&rule_name.name);

    // When the ruleset is finalized, this interface name is replaced by actual interface name
    // Setup interface placeholder
    let mut interface_idents = vec![];
    for interface in &rule_expr.interface {
        // Interface wrapper
        if !ruleset_factory
            .lock()
            .unwrap()
            .exist_interface(&*interface.name)
        {
            return Err(RuLaCompileError::NoInterfaceFoundError);
        }
        let target_interface = ruleset_factory
            .lock()
            .unwrap()
            .get_interface(&*interface.name);
        // Providing ref would be better
        // This can be just a reference to the interface
        let gen_str = SynLit::Str(LitStr::new(&*interface.name, Span::call_site()));
        interface_idents.push(quote!(#gen_str.to_string()));
        rule.add_interface(&interface.name, target_interface);
    }

    // Set empty condition and action to be updated in the different functions
    // Clauses are added directly to this empty condition and action
    let empty_condition = Condition::new(None);
    let empty_action = Action::<ActionClauses>::new(None);
    rule.set_condition(empty_condition);
    rule.set_action(empty_action);

    ruleset_factory
        .lock()
        .unwrap()
        .add_rule(&rule_name_string, rule);

    let mut generated_args = vec![];
    let mut argument_adder = vec![];
    for arg in &mut rule_expr.args {
        ruleset_factory
            .lock()
            .unwrap()
            .add_rule_arg(&rule_name_string, &arg.name);
        ident_tracker.register(
            &*arg.name,
            Identifier::new(IdentType::RuleArgument, helper::type_filler(&arg)),
        );
        arg.update_ident_type(IdentType::RuleArgument);
        let arg_string = SynLit::Str(LitStr::new(&arg.name, Span::call_site()));
        generated_args.push(arg_string.clone());

        // let gen_ident = format_ident!("{}", &*arg.name);
        // let (val_type, gen_type_hint) = helper::arg_val_type_token_gen(&arg);
        argument_adder.push(quote!(
                let arg = Argument::init();
                empty_argument.insert(#arg_string.to_string(), arg);
        ))
    }

    // 2. Generate executable Rule
    // generate the rule content expression with this
    let generated = generate_rule_content(
        &mut rule_expr.rule_content,
        Some(&rule_name_string),
        ident_tracker,
    )
    .unwrap();

    // RuLa runtime generation
    let rule_name_token = generate_ident(rule_name, ident_tracker, false, false).unwrap();
    let unready_rule_name_token = format_ident!("unready_{}", &*rule_name.name);

    let static_act = generate_static_act(
        &mut *rule_expr.rule_content.action_expr,
        Some(&*rule_name.name),
        ident_tracker,
    )
    .unwrap();
    let static_cond = generate_static_cond(
        &mut *rule_expr.rule_content.condition_expr,
        Some(&*rule_name.name),
        ident_tracker,
    )
    .unwrap();

    Ok(quote!(
        pub struct #rule_name_token{
            interfaces: Vec<String>,
            arguments: HashMap<String, Argument>,
        }

        pub struct #unready_rule_name_token{
            interfaces: Vec<String>,
            // Should copy interface information laters
            static_interfaces: HashMap<String, QnicInterface>,
            arguments: HashMap<String, Argument>,
            condition_clauses: Vec<ConditionClauses>,
            action_clauses: Vec<ActionClausesV2>,
        }

        impl #unready_rule_name_token{
            // pub fn new(args_from_prev_rule: Option<HashMap<String, Argument>>, config: Option<&CONFIG>) -> Self{
            // pub fn new(arguments: Option<HashMap<String, Argument>>) -> Self{
            pub async fn new() -> #unready_rule_name_token{
                // 1. prepare empty arguments based on arugment list
                let mut empty_argument = HashMap::new();
                #(#argument_adder);*;

                let mut static_interfaces = HashMap::new();
                let interface_list = INTERFACES.get().expect("Unable to find interface table");
                for i_name in vec![#(#interface_idents), *].iter(){
                    let interface_def = interface_list.lock().await.get(i_name).expect("Unable to get interface").clone();
                    // let interface_def = QnicInterface::generate_mock_interface(i_name, 10);
                    static_interfaces.insert(i_name.to_string(), interface_def);
                }
                // 2. return structure
                #unready_rule_name_token{
                    interfaces: vec![#(#interface_idents), *],
                    static_interfaces: static_interfaces,
                    arguments: empty_argument,
                    condition_clauses: vec![],
                    action_clauses: vec![],
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
                        interfaces: self.interfaces.clone(),
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
            fn static_ruleset_gen(&mut self){
                let mut interface_map = HashMap::<String, InterfaceInfo>::new();
                for i_name in &self.interfaces {
                    interface_map.insert(i_name.to_string(), __get_interface_info(i_name));
                }
                let condition_clauses = Rc::new(RefCell::new(vec![]));
                let action_clauses = Rc::new(RefCell::new(vec![]));
                #static_cond
                #static_act
            }

            fn gen_ruleset(&mut self, ruleset: &mut RuleSet<ActionClausesV2>){
                self.static_ruleset_gen();
                let mut rule = Rule::<ActionClausesV2>::new(#rule_name_string);
                let mut condition = Condition::new(None);
                let mut action = Action::new(None);
                for cond in &self.condition_clauses{
                    condition.add_condition_clause(cond.clone());
                }
                for act in &self.action_clauses{
                    action.add_action_clause(act.clone());
                }
                rule.set_condition(condition);
                rule.set_action(action);
                ruleset.add_rule(rule);
            }

        }
        // #[async_trait]
        // impl Rulable for #rule_name_token{
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
        post_processes.push(generate_stmt(stmt, None, ident_tracker, false).unwrap());
    }
    // Generate executable
    // Here we should consider to use TaskMonitor in tokio to track the status of watched values
    Ok(quote!(
        async fn condition(&self) -> bool{
            let mut interface = INTERFACES.get().expect("Unable to get interface table").lock().await;
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
    // watch:
    //      let qubit = qn0.get_qubit();
    let mut generated_watch = vec![];
    // let mut watched_values = vec![];
    match rule_name {
        Some(name) => {
            for value in &mut watch_expr.watched_values {
                // start watch value generation process with `in_watch=true`
                // watched_values.push(generate_ident(&value.ident).unwrap());
                value.ident.update_ident_type(IdentType::WatchedValue);
                generated_watch.push(
                    generate_let(
                        value,
                        Some(&name),
                        true,
                        ident_tracker,
                        in_static,
                        &mut vec![],
                    )
                    .unwrap(),
                );
                // watch can be
            }
        }
        None => todo!(),
    };

    // Generate
    // This could be on the tokio async runtime
    Ok(quote!(
        #(#generated_watch)*
    ))
}

pub(super) fn generate_cond(
    cond_expr: &mut CondExpr,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
    act_tokens: &TokenStream,
) -> IResult<TokenStream> {
    let mut generated_clauses = vec![];
    let (in_rule, r_name) = helper::check_in(rule_name);

    // Sweep Rule meta here to get the condition clause information and add it
    // At this moment all information that needs to be considered should be inside the RuleMeta
    // However, some of them are given by configs.
    if in_rule {
        for clause in &mut cond_expr.clauses {
            generated_clauses
                .push(generate_awaitable(clause, Some(&r_name), ident_tracker, false).unwrap());
        }
        match &mut *cond_expr.watch_expr {
            Some(watch_expr) => {
                let generated_watch =
                    generate_watch(watch_expr, Some(&r_name), ident_tracker, false).unwrap();
                Ok(quote!(
                   #generated_watch
                   if #(#generated_clauses)&&*{
                    // self.action().await;
                    (||async {
                        #act_tokens
                    })();
                    return true
                   }else{
                    return false
                   };
                ))
            }
            None => Ok(quote!(
               if #(#generated_clauses)&&*{
                (||async {
                    #act_tokens
                })();
                return true
               }else{
                return false
               };
            )),
        }
    } else {
        todo!()
    }
}

pub(super) fn generate_awaitable(
    awaitable_expr: &mut Awaitable,
    rule_name: Option<&String>,
    ident_tracker: &mut IdentTracker,
    in_static: bool,
) -> IResult<TokenStream> {
    // Generating condition clauses to be met
    let (in_rule, r_name) = helper::check_in(rule_name);
    // Start creating a new condition and replace the old one with new condition?
    let mut awaitable = quote!();
    if in_rule {
        awaitable = match awaitable_expr {
            Awaitable::FnCall(fn_call) => {
                // This should be flex
                let generated_fn_call =
                    generate_fn_call(fn_call, rule_name, ident_tracker, true, in_static).unwrap();
                quote!(
                    #generated_fn_call
                )
            }
            Awaitable::VariableCallExpr(val_call) => {
                // Check if the value is properly watched or not
                let generated_val =
                    generate_variable_call(val_call, Some(&r_name), ident_tracker, in_static)
                        .unwrap();
                if in_static {
                    quote!(#generated_val)
                } else {
                    quote!(
                            #generated_val.await
                    )
                }
            }
            Awaitable::Comp(comp) => {
                let generated_comp = generate_comp(comp, rule_name, ident_tracker).unwrap();
                quote!(
                    (||{ #generated_comp })()
                )
            }
        }
    } else {
        todo!("Currently, non ruleset generation version has not yet been supported")
    }
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
            StmtKind::Let(let_stmt) => action_calls.push(
                generate_let(
                    let_stmt,
                    rule_name,
                    false,
                    ident_tracker,
                    false,
                    &mut vec![],
                )
                .unwrap(),
            ),
            _ => {
                let generated = generate_stmt(action, rule_name, ident_tracker, false).unwrap();
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

    let mut action_collected = vec![];
    for action in &mut *action_expr.operatable {
        match &mut *action.kind {
            StmtKind::Let(let_stmt) => static_action_calls.push(
                generate_let(
                    let_stmt,
                    rule_name,
                    false,
                    ident_tracker,
                    true,
                    &mut action_collected,
                )
                .unwrap(),
            ),
            _ => {
                let generated = generate_stmt(action, rule_name, ident_tracker, true).unwrap();
                static_action_calls.push(quote!(
                    #generated;
                ))
            }
        }
    }
    let mut final_action_collection = vec![];
    for coll in action_collected {
        final_action_collection.push(quote!(for action in &#coll.generated_actions {
            self.action_clauses.push(action.clone())
        }))
    }
    Ok(quote!(
        #(#static_action_calls);*
        #(#final_action_collection)*
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
    in_static: bool,
) -> IResult<TokenStream> {
    let mut variable_calls = vec![];

    if variable_call_expr.variables.len() == 0 {
        panic!("Internal Error. This needs to be treated in parser.")
    }
    for (index, val) in &mut variable_call_expr.variables.iter().enumerate() {
        let specified = if index == 0 {
            false
        } else {
            match &variable_call_expr.variables[index - 1] {
                Callable::Ident(ident) => {
                    // If the variable is registered as qubit, operations should be awaited
                    match ident_tracker.check_ident_type(&ident.name) {
                        IdentType::QubitInterface => true,
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
        items.push(generate_lit(lits, ident_tracker, false).unwrap());
    }
    Ok(quote!(vec![#(#items),*]))
}

pub(super) fn generate_lit(
    lit: &Lit,
    ident_tracker: &mut IdentTracker,
    in_static: bool,
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
            Ok(quote!(#val))
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
            ident_tracker.add_interface_name(&*ident.name);
            let ident_str = SynLit::Str(LitStr::new(&*ident.name, Span::call_site()));
            if !in_static {
                quote!(
                    interface.get(#ident_str)
                    .expect("unable to get interface")
                )
            } else {
                // In static
                quote!(
                    self.static_interfaces.get(#ident_str).expect("Unable to get Interface")
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
        TypeDef::Integer32 => Ok(quote!(i32)),
        TypeDef::Integer64 => Ok(quote!(i64)),
        TypeDef::UnsignedInteger32 => Ok(quote!(u32)),
        TypeDef::UnsignedInteger64 => Ok(quote!(u64)),
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
                    TypeDef::Integer32 => TypeHint::Integer64,
                    TypeDef::Integer64 => TypeHint::Integer64,
                    TypeDef::Float32 => TypeHint::Float64,
                    TypeDef::Float64 => TypeHint::Float64,
                    TypeDef::Str => TypeHint::Str,
                    TypeDef::UnsignedInteger32 => TypeHint::UnsignedInteger64,
                    TypeDef::UnsignedInteger64 => TypeHint::UnsignedInteger64,
                    TypeDef::Boolean => TypeHint::Boolean,
                    TypeDef::Vector(inner) => match &**inner {
                        TypeDef::Integer32 => TypeHint::I64Vector,
                        TypeDef::Integer64 => TypeHint::I64Vector,
                        TypeDef::Float32 => TypeHint::F64Vector,
                        TypeDef::Float64 => TypeHint::F64Vector,
                        TypeDef::Str => TypeHint::StrVector,
                        TypeDef::UnsignedInteger32 => TypeHint::U64Vector,
                        TypeDef::UnsignedInteger64 => TypeHint::U64Vector,
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
                    TypeDef::Integer32 => (quote!(Integer64), quote!(LibTypeHint::Integer64)),
                    TypeDef::Integer64 => (quote!(Integer64), quote!(LibTypeHint::Integer64)),
                    TypeDef::Float32 => (quote!(Float64), quote!(LibTypeHint::Float64)),
                    TypeDef::Float64 => (quote!(Float64), quote!(LibTypeHint::Float64)),
                    TypeDef::Str => (quote!(Str), quote!(LibTypeHint::Str)),
                    TypeDef::UnsignedInteger32 => (
                        quote!(UnsignedInteger64),
                        quote!(LibTypeHint::UnsignedInteger64),
                    ),
                    TypeDef::UnsignedInteger64 => (
                        quote!(UnsignedInteger64),
                        quote!(LibTypeHint::UnsignedInteger64),
                    ),
                    TypeDef::Boolean => (quote!(Boolean), quote!(TypeHint::Boolean)),
                    TypeDef::Vector(content_type) => match &**content_type {
                        TypeDef::Integer32 => (
                            quote!(I64Vector),
                            quote!(LibTypeHint::Vector(Box::new(LibTypeHint::Integer64))),
                        ),
                        TypeDef::Integer64 => (
                            quote!(I64Vector),
                            quote!(LibTypeHint::Vector(Box::new(LibTypeHint::Integer64))),
                        ),
                        TypeDef::UnsignedInteger32 => (
                            quote!(U64Vector),
                            quote!(LibTypeHint::Vector(Box::new(
                                LibTypeHint::UnsignedInteger64
                            ))),
                        ),
                        TypeDef::UnsignedInteger64 => (
                            quote!(U64Vector),
                            quote!(LibTypeHint::Vector(Box::new(
                                LibTypeHint::UnsignedInteger64
                            ))),
                        ),
                        TypeDef::Float32 => (
                            quote!(F64Vector),
                            quote!(LibTypeHint::Vector(Box::new(LibTypeHint::Float64))),
                        ),
                        TypeDef::Float64 => (
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
