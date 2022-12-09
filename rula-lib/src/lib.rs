#![allow(non_snake_case)]
pub mod message;
pub mod operation;
pub mod prelude;
pub mod qnic;
pub mod qubit;
pub mod result;
pub mod time;

pub mod ruleset {
    pub mod action;
    pub mod condition;
    pub mod ruleset;
}

use ruleset::action::v2::ActionClauses;
use ruleset::ruleset::Rule;
use std::cell::RefCell;
use std::rc::Rc;
pub type RuleVec = Rc<RefCell<Vec<RefCell<Rule<ActionClauses>>>>>;
