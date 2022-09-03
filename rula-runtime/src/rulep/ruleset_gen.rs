use serde::{Deserialize, Serialize};
use serde_json::json;
use std::net::IpAddr;

pub fn generate_ruleset() {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make_json() {
        let ruleset = generate_ruleset();
    }
}
