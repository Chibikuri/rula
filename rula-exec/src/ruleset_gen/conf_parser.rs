use super::repeater::Repeater;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct RepeaterConfigs {
    pub repeaters: Vec<Repeater>,
}

pub fn parse_config(path: PathBuf) -> u32 {
    1
}
