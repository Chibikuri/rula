use super::types::Repeater;
use super::IResult;
use crate::ruleset_gen::ruleset::AddressKind;

use serde::{Deserialize, Serialize};

use std::fs::File;
use std::io::BufReader;
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RepeaterConfigs {
    pub repeaters: Vec<RepeaterContent>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RepeaterContent {
    pub name: String,
    pub address: AddressKind,
}

pub fn parse_config(file_path: &PathBuf) -> IResult<Vec<Repeater>> {
    let config = deserialize_config(file_path).unwrap();
    // Initiator (#1) < --- > Responder (#fin)
    // 1. Generate all repeaters
    let mut repeaters = vec![];
    for (index, content) in config.repeaters.iter().enumerate() {
        let mut repeater = Repeater::new(&content.name);
        repeater.update_index(index as u64);
        repeater.update_address(content.address.clone());
        repeaters.push(repeater);
    }
    let repeater_copy = repeaters.clone();
    let rep_size = repeaters.len();

    let mut return_repeaters = vec![];
    // 2. distance resolve (push partners)
    for (i, rep) in repeaters.iter_mut().enumerate() {
        // Vector index is corresponding to the distance
        // Add repeater until j gets to 0 (reverse)
        for j in (0..i).rev() {
            rep.add_left_repeater(&repeater_copy[j])
        }
        // Add repeater until k gets to max
        for k in i + 1..rep_size {
            rep.add_right_repeater(&repeater_copy[k])
        }
        return_repeaters.push(rep.clone());
    }
    Ok(return_repeaters)
}

fn deserialize_config(file_path: &PathBuf) -> IResult<RepeaterConfigs> {
    let file = File::open(file_path).expect("No such file");
    let reader = BufReader::new(file);

    let deserialized: RepeaterConfigs = serde_json::from_reader(reader).unwrap();
    Ok(deserialized)
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::ruleset_gen::tracker::Tracker;
    use std::cell::RefCell;

    #[test]
    fn test_parse_config() {
        let config_path = PathBuf::from("../examples/v2/config.json");
        let deserialized = deserialize_config(&config_path).unwrap();
        assert_eq!(deserialized.repeaters.len(), 5);
        assert_eq!(deserialized.repeaters[0].name, "#1");
        assert_eq!(deserialized.repeaters[1].name, "#2");
        assert_eq!(deserialized.repeaters[2].name, "#3");
    }

    #[test]
    fn test_repeater_resolve() {
        // #1 < -- > #2 < -- > #3 < -- > #4 < -- > #5
        let config_path = PathBuf::from("../examples/v2/config.json");
        let _tracker = RefCell::new(Tracker::new());
        let repeater_list = parse_config(&config_path).unwrap();
        assert_eq!(repeater_list.len(), 5);

        // The most left repeater (Initiator)
        assert_eq!(repeater_list[0].left_repeaters.len(), 0);
        assert_eq!(repeater_list[0].right_repeaters.len(), 4);
        assert_eq!(repeater_list[0].right_repeaters[0].name, "#2");

        // Middle repeater
        assert_eq!(repeater_list[2].left_repeaters.len(), 2);
        assert_eq!(repeater_list[2].left_repeaters[0].name, "#2");
        assert_eq!(repeater_list[2].left_repeaters[1].name, "#1");
        assert_eq!(repeater_list[2].right_repeaters.len(), 2);
        assert_eq!(repeater_list[2].right_repeaters[0].name, "#4");
        assert_eq!(repeater_list[2].right_repeaters[1].name, "#5");

        // Last repeater (Responder)
        assert_eq!(repeater_list[4].left_repeaters.len(), 4);
        assert_eq!(repeater_list[4].right_repeaters.len(), 0);
        assert_eq!(repeater_list[4].left_repeaters[0].name, "#4");
    }
}
