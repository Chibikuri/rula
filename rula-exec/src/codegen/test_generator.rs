// use super::generator::*;
// use super::identifier::*;
// use rula_parser::parser::ast::*;

// #[cfg(test)]
// mod test_let_stmt_gen {
//     use super::*;
//     #[test]
//     fn test_simple_let_stmt() {
//         // let x:u_int = test_gen();
//         let mut test_ast = Let::new(
//             Ident::new("x", Some(TypeDef::UnsignedInteger), IdentType::Other),
//             Expr::new(ExprKind::FnCall(FnCall::new(
//                 Ident::new("test_gen", None, IdentType::Other),
//                 vec![],
//             ))),
//         );
//         let mut tracker = IdentTracker::new();
//         tracker.register("x", Identifier::new(IdentType::Other, TypeHint::Unknown));
//         let generated_let = generate_let(&mut test_ast, None, &mut tracker, false, false).unwrap();
//         assert_eq!("let mut x : u64 = test_gen () ;", generated_let.to_string());
//     }

//     #[test]
//     #[ignore = "error occur in singleton generation"]
//     fn test_static_let_stmt() {
//         // generate static function
//         // let x : u_int = test_gen(); --> let x : u32 = __static__test_gen();
//         let mut test_ast = Let::new(
//             Ident::new("x", Some(TypeDef::UnsignedInteger), IdentType::Other),
//             Expr::new(ExprKind::FnCall(FnCall::new(
//                 Ident::new("test_gen", None, IdentType::Other),
//                 vec![],
//             ))),
//         );
//         let mut tracker = IdentTracker::new();
//         tracker.register("x", Identifier::new(IdentType::Other, TypeHint::Unknown));
//         let generated_let = generate_let(&mut test_ast, None, &mut tracker, false, true).unwrap();
//         assert_eq!(
//             "let mut x = __static__test_gen () . clone () ;",
//             generated_let.to_string()
//         );
//     }

//     #[test]
//     #[ignore]
//     fn test_watch_let_stmt() {
//         // generate watched values with let stmt
//         /*
//          * watch{
//          *   let test = qn0.request_resource(1, qn0.get_partner_by_hop(distance));
//          * }
//          */
//         let mut test_ast = Let::new(
//             Ident::new("test", None, IdentType::WatchedValue),
//             Expr::new(ExprKind::VariableCallExpr(VariableCallExpr::new(vec![
//                 Callable::Ident(Ident::new("qn0", None, IdentType::QnicInterface)),
//                 Callable::FnCall(FnCall::new(
//                     Ident::new("request_resource", None, IdentType::Other),
//                     vec![
//                         Expr::new(ExprKind::Lit(Lit::new(LitKind::NumberLit(NumberLit::new(
//                             "1",
//                         ))))),
//                         Expr::new(ExprKind::VariableCallExpr(VariableCallExpr::new(vec![
//                             Callable::Ident(Ident::new("qn0", None, IdentType::QnicInterface)),
//                             Callable::FnCall(FnCall::new(
//                                 Ident::new("get_partner_by_hop", None, IdentType::Other),
//                                 vec![Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(
//                                     Ident::new("distance", None, IdentType::RuleArgument),
//                                 ))))],
//                             )),
//                         ]))),
//                     ],
//                 )),
//             ]))),
//         );

//         let mut tracker = IdentTracker::new();
//         tracker.register("test", Identifier::new(IdentType::Other, TypeHint::Unknown));
//         tracker.register(
//             "request_resource",
//             Identifier::new(IdentType::Other, TypeHint::Unknown),
//         );
//         tracker.register(
//             "get_partner_by_hop",
//             Identifier::new(IdentType::Other, TypeHint::Unknown),
//         );
//         tracker.register(
//             "distance",
//             Identifier::new(IdentType::RuleArgument, TypeHint::Unknown),
//         );
//         // let generated_let = generate_let(&mut test_ast, None, true, &mut tracker, false, &mut vec![]).unwrap();
//         // assert_eq!(
//         //     "let mut test = ",
//         //     generated_let.to_string(),
//         // )
//     }
// }

// // ident tests
// #[test]
// fn test_ident_no_type_hint() {
//     let test_ident = Ident::new("hello", None, IdentType::Other);
//     let mut tracker = IdentTracker::new();
//     tracker.register(
//         "hello",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     let test_stream = generate_ident(&test_ident, &mut tracker, false, false).unwrap();
//     assert_eq!("hello", test_stream.to_string());
// }
// #[test]
// fn test_ident_with_type_hint() {
//     let test_ident = Ident::new("hello", Some(TypeDef::Boolean), IdentType::Other);
//     let mut tracker = IdentTracker::new();
//     tracker.register(
//         "hello",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     let test_stream = generate_ident(&test_ident, &mut tracker, true, false).unwrap();
//     assert_eq!("hello : bool", test_stream.to_string());
// }

// // Import test
// #[test]
// fn test_simple_import() {
//     // assert!(IDENT_TABLE.get().is_none());
//     // let initialize_ident_table = || Mutex::new(HashMap::<String, IdentType>::new());
//     // let _ = IDENT_TABLE.get_or_init(initialize_ident_table);
//     // import hello;
//     let expected_paths = vec![["hello"].iter().collect()];
//     let mut test_import = Import::new(PathKind::from(expected_paths));
//     let mut tracker = IdentTracker::new();
//     tracker.register(
//         "hello",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     let test_stream = generate_import(&mut test_import, &mut tracker).unwrap();
//     assert_eq!("use hello ;", test_stream.to_string());
// }

// // If test
// #[test]
// fn test_simple_if() {
//     // if (block) {expression}
//     let mut simple_if = If::new(
//         // (block)
//         Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
//             "block",
//             None,
//             IdentType::Other,
//         ))))),
//         // {expression}
//         vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
//             Lit::new(LitKind::Ident(Ident::new(
//                 "expression",
//                 None,
//                 IdentType::Other,
//             ))),
//         ))))],
//         // elif ~
//         vec![],
//         // else ~
//         None,
//     );
//     let mut tracker = IdentTracker::new();
//     tracker.register(
//         "block",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     tracker.register(
//         "expression",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     let test_stream = generate_if(&mut simple_if, &mut tracker, false, false).unwrap();
//     assert_eq!("if block { expression }", test_stream.to_string());
// }

// #[test]
// fn test_if_else() {
//     // initialize_singleton();
//     // if (block) {expression} else {expression2}
//     let mut if_else = If::new(
//         // (block)
//         Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
//             "block",
//             None,
//             IdentType::Other,
//         ))))),
//         // {expression}
//         vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
//             Lit::new(LitKind::Ident(Ident::new(
//                 "expression",
//                 None,
//                 IdentType::Other,
//             ))),
//         ))))],
//         // elif ~
//         vec![],
//         // else ~
//         Some(Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
//             Lit::new(LitKind::Ident(Ident::new(
//                 "expression2",
//                 None,
//                 IdentType::Other,
//             ))),
//         ))))),
//     );
//     let mut tracker = IdentTracker::new();
//     tracker.register(
//         "block",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     tracker.register(
//         "expression",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     tracker.register(
//         "expression2",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     let test_stream = generate_if(&mut if_else, &mut tracker, false, false).unwrap();
//     assert_eq!(
//         "if block { expression } else { expression2 }",
//         test_stream.to_string()
//     );
// }

// #[test]
// fn test_if_elif_else() {
//     // initialize_singleton();
//     // if(block){expression} else if (block2){expression2} else {expression3}
//     let mut if_elif_else = If::new(
//         // (block)
//         Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
//             "block",
//             None,
//             IdentType::Other,
//         ))))),
//         // {expression}
//         vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
//             Lit::new(LitKind::Ident(Ident::new(
//                 "expression",
//                 None,
//                 IdentType::Other,
//             ))),
//         ))))],
//         // elif ~
//         vec![Some(If::new(
//             // else if (block)
//             Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
//                 "block2",
//                 None,
//                 IdentType::Other,
//             ))))),
//             // else if () {statement2;};
//             vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
//                 Lit::new(LitKind::Ident(Ident::new(
//                     "expression2",
//                     None,
//                     IdentType::Other,
//                 ))),
//             ))))],
//             vec![],
//             None,
//         ))],
//         // else ~
//         Some(Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
//             Lit::new(LitKind::Ident(Ident::new(
//                 "expression3",
//                 None,
//                 IdentType::Other,
//             ))),
//         ))))),
//     );
//     let mut tracker = IdentTracker::new();
//     tracker.register(
//         "block",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     tracker.register(
//         "block2",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     tracker.register(
//         "expression",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     tracker.register(
//         "expression2",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     tracker.register(
//         "expression3",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     let test_stream = generate_if(&mut if_elif_else, &mut tracker, false, false).unwrap();
//     assert_eq!(
//         "if block { expression } else if block2 { expression2 } else { expression3 }",
//         test_stream.to_string()
//     );
// }

// // for test
// #[test]
// fn test_simple_for_generation() {
//     // for (i) in generator {hello}
//     let mut simple_for = For::new(
//         vec![Ident::new("i", None, IdentType::Other)],
//         Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
//             "generator",
//             None,
//             IdentType::Other,
//         ))))),
//         vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
//             Lit::new(LitKind::Ident(Ident::new("hello", None, IdentType::Other))),
//         ))))],
//     );

//     let mut tracker = IdentTracker::new();
//     tracker.register("i", Identifier::new(IdentType::Other, TypeHint::Unknown));
//     tracker.register(
//         "generator",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     tracker.register(
//         "hello",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     let test_stream = generate_for(&mut simple_for, &mut tracker).unwrap();
//     assert_eq!("for i in generator { hello }", test_stream.to_string());
// }

// #[test]
// fn test_multi_arg_for_generation() {
//     // for (a, b, c) in generator{hello}
//     let mut multi_for = For::new(
//         vec![
//             Ident::new("a", None, IdentType::Other),
//             Ident::new("b", None, IdentType::Other),
//             Ident::new("c", None, IdentType::Other),
//         ],
//         Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
//             "generator",
//             None,
//             IdentType::Other,
//         ))))),
//         vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
//             Lit::new(LitKind::Ident(Ident::new("hello", None, IdentType::Other))),
//         ))))],
//     );
//     let mut tracker = IdentTracker::new();
//     tracker.register("a", Identifier::new(IdentType::Other, TypeHint::Unknown));
//     tracker.register("b", Identifier::new(IdentType::Other, TypeHint::Unknown));
//     tracker.register("c", Identifier::new(IdentType::Other, TypeHint::Unknown));
//     tracker.register(
//         "generator",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     tracker.register(
//         "hello",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     let test_stream = generate_for(&mut multi_for, &mut tracker).unwrap();
//     assert_eq!(
//         "for (a , b , c) in generator { hello }",
//         test_stream.to_string()
//     );
// }
// // While test
// #[test]
// fn test_simple_while() {
//     let mut simple_while = While::new(
//         Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
//             "count",
//             None,
//             IdentType::Other,
//         ))))),
//         Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
//             LitKind::Ident(Ident::new("expression", None, IdentType::Other)),
//         ))))),
//     );
//     let mut tracker = IdentTracker::new();
//     tracker.register(
//         "count",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     tracker.register(
//         "expression",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     let test_stream = generate_while(&mut simple_while, &mut tracker).unwrap();
//     assert_eq!("while count { expression }", test_stream.to_string());
// }

// // FnDef test
// #[test]
// fn test_simple_fn_def() {
//     // fn(block:int, hello:str){expression}
//     let mut simple_fn_def = FnDef::new(
//         vec![
//             Ident::new("block", Some(TypeDef::Integer), IdentType::Other),
//             Ident::new("hello", Some(TypeDef::Str), IdentType::Other),
//         ],
//         Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(Lit::new(
//             LitKind::Ident(Ident::new("expression", None, IdentType::Other)),
//         ))))),
//     );
//     let mut tracker = IdentTracker::new();
//     tracker.register(
//         "block",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     tracker.register(
//         "hello",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     tracker.register(
//         "expression",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     let test_stream = generate_fn_def(&mut simple_fn_def, &mut tracker).unwrap();
//     assert_eq!(
//         "pub fn (block : i64 , hello : String) { expression }",
//         test_stream.to_string()
//     );
// }

// // FnCall test
// #[test]
// fn test_simple_fn_call() {
//     // range()
//     let mut simple_fn_call = FnCall::new(Ident::new("range", None, IdentType::Other), vec![]);
//     let mut tracker = IdentTracker::new();
//     tracker.register(
//         "range",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     let test_stream =
//         generate_fn_call(&mut simple_fn_call, None, &mut tracker, false, false, false).unwrap();
//     assert_eq!("range ()", test_stream.to_string());
// }

// // Struct test
// #[test]
// fn test_simple_struct() {
//     // struct Test{flag: bool}
//     let simple_struct = Struct::new(
//         Ident::new("Test", None, IdentType::Other),
//         vec![Ident::new("flag", Some(TypeDef::Boolean), IdentType::Other)],
//     );
//     let mut tracker = IdentTracker::new();
//     tracker.register("Test", Identifier::new(IdentType::Other, TypeHint::Unknown));
//     tracker.register("flag", Identifier::new(IdentType::Other, TypeHint::Unknown));
//     let test_stream = generate_struct(&simple_struct, &mut tracker).unwrap();
//     assert_eq!("struct Test { flag : bool }", test_stream.to_string());
// }

// // Return test
// // #[test]
// // fn test_simple_return() {
// //     // return hello
// //     let mut simple_return = Return::new(Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(
// //         Ident::new("hello", None, IdentType::Other),
// //     )))));
// //     let mut tracker = IdentTracker::new();
// //     tracker.register(
// //         "hello",
// //         Identifier::new(IdentType::Other, TypeHint::Unknown),
// //     );
// //     let test_stream = generate_return(&mut simple_return, &mut tracker, false).unwrap();
// //     assert_eq!("return hello ;", test_stream.to_string());
// // }

// // Comp expr test
// #[test]
// fn test_simple_comp() {
//     // count > prev_count
//     let mut comp_expr = Comp::new(
//         Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
//             "count",
//             None,
//             IdentType::Other,
//         ))))),
//         CompOpKind::Gt,
//         Expr::new(ExprKind::Lit(Lit::new(LitKind::Ident(Ident::new(
//             "prev_count",
//             None,
//             IdentType::Other,
//         ))))),
//     );
//     let mut tracker = IdentTracker::new();
//     tracker.register(
//         "count",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     tracker.register(
//         "prev_count",
//         Identifier::new(IdentType::Other, TypeHint::Unknown),
//     );
//     let test_stream =
//         generate_comp(&mut comp_expr, None, &mut tracker, false, false, false).unwrap();
//     assert_eq!(
//         "__comp (count , __CmpOp :: Gt , prev_count)",
//         test_stream.to_string()
//     );
// }

// #[test]
// fn test_simple_array() {
//     // [1, 2, 3, 4, 5]
//     let array_expr = Array::new(vec![
//         Lit::new(LitKind::NumberLit(NumberLit::new("1"))),
//         Lit::new(LitKind::NumberLit(NumberLit::new("2"))),
//         Lit::new(LitKind::NumberLit(NumberLit::new("3"))),
//         Lit::new(LitKind::NumberLit(NumberLit::new("4"))),
//         Lit::new(LitKind::NumberLit(NumberLit::new("5"))),
//     ]);
//     let test_stream = generate_array(&array_expr, &mut IdentTracker::new()).unwrap();
//     assert_eq!("vec ! [1 , 2 , 3 , 4 , 5]", test_stream.to_string());
// }

// #[test]
// fn test_simple_rule() {
//     // rule hello<qn0>(q2: Qubit){expression}
//     // let rule_expr = RuleExpr::new(
//     //     Ident::new("hello", None),
//     //     vec![Ident::new("qn0", None)],
//     //     vec![Ident::new("q2", Some(TypeDef::Qubit))],
//     //     vec![Stmt::new(StmtKind::Expr(Expr::new(ExprKind::Lit(
//     //         Lit::new(LitKind::Ident(Ident::new("expression", None))),
//     //     ))))],
//     // );
//     // let test_stream = generate_rule(&rule_expr).unwrap();
//     // assert_eq!(
//     //     "rule hello<qn0>(q2: Qubit){expression}",
//     //     test_stream.to_string()
//     // );
// }

// #[test]
// fn test_interface_generation() {
//     // #interface: {qn0, qn1} => qnall;
// }
