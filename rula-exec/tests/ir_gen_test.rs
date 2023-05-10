use rula_exec::ruleset_gen::generator::*;

#[cfg(test)]
mod test_generate_rula_ir {
    use rula_parser::parser::ast::*;

    use super::*;

    #[test]
    fn test_rula_ir() {
        let rula = RuLa::new(RuLaKind::Ignore);
        // let test = generate_rula_ir(&rula).unwrap();
        // for ir in test.iter(){
        //     println!("{:#?}", ir);
        // }
    }
}
