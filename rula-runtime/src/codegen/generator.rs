// This is entory point to generate code from AST

use rula::parser::ast::*;

// generate corresponding rust code from ast
pub fn generate(ast_tree: Vec<AstNode>) {
    for ast_node in ast_tree {
        match ast_node{
            AstNode::RuLa(rula) => {
                println!("hello{:#?}", rula);
            },
            AstNode::PlaceHolder => {
                panic!("Value not properly set");
            }
        }
    }
}


fn generate_rula<T: GenRust>(rula: T) -> RuLaProgram {
    let rula = RuLaProgram;
    rula
}

// trait generates rust code
pub trait GenRust {
    fn gen_rust(&self) -> String;
}

struct RuLaProgram;

impl GenRust for RuLaProgram {
    fn gen_rust(&self) -> String {
        String::from("")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_generation() {
        let target_ast = vec![AstNode::RuLa(RuLa::place_holder())];
        generate(target_ast);
    }
}
