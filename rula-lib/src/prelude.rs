use rula_exec::ruleset_gen::types::Repeater;

#[derive(Debug, PartialEq, Clone)]
pub struct RepeaterList {
    pub repeaters: Vec<Repeater>,
}

impl RepeaterList {
    pub fn len(&self) -> usize {
        self.repeaters.len()
    }
}
