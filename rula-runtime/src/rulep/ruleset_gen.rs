use serde::{Deserialize, Serialize};
use std::net::IpAddr;
use serde_json::json;



pub fn generate_ruleset() {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make_json() {
        let ruleset = generate_ruleset();
    }
}
