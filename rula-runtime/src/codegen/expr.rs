use rula::parser::ast::*;

fn gen_if_expr(node: ExprKind){
    match node {
        ExprKind::Import(import) => {
            println!("import statement {:#?}", import);
        },
        ExprKind::If(if_expr) => {
            println!("if expression {:#?}", if_expr);
        }
        _ => unreachable!("Unreachable!")
    }
}