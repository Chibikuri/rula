use proc_macro2::TokenStream;

pub fn default_imports() -> TokenStream {
    quote!(
        use super::*;
        use std::collections::{HashSet, HashMap};
        use std::iter::FromIterator;
        use std::cell::RefCell;
        use std::rc::Rc;
        use std::sync::Mutex as StdMutex;
        use serde::{Deserialize, Serialize};
        use tokio::time::{sleep, Duration};
        use rula_std::prelude::*;
        use rula_std::message::*;
        use rula_std::result::*;
        use rula_std::operation::*;
        use rula_std::qnic::*;
        use rula_std::qubit::*;
        use rula_std::ruleset::ruleset::*;
        use rula_std::ruleset::action::v2::ActionClauses as ActionClausesV2;
        use rula_std::ruleset::condition::v1::ConditionClauses;
        use rula_std::ruleset::condition::v1::*;
        use rula_std::RuleVec;
        use tokio::sync::Mutex as TokioMutex;
        use once_cell::sync::OnceCell;
    )
}
