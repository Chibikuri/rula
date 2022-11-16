use proc_macro2::TokenStream;

pub fn default_imports() -> TokenStream {
    quote!(
        use super::*;
        use std::collections::{HashSet, HashMap};
        use std::iter::FromIterator;
        use std::borrow::BorrowMut;
        use std::cell::{RefCell, Cell};
        use std::rc::Rc;
        use std::sync::Mutex as StdMutex;
        use serde::{Deserialize, Serialize};
        use tokio::time::{sleep, Duration};
        use rula_std::rule::*;
        use rula_std::prelude::*;
        use rula_std::message::*;
        use rula_std::result::*;
        use rula_std::operation::*;
        use rula_std::ruleset::ruleset::*;
        use rula_std::ruleset::condition::*;
        use rula_std::ruleset::action::Action;
        use rula_std::ruleset::action::v2::ActionClauses as ActionClausesV2;
        use rula_std::ruleset::condition::v1::ConditionClauses;
        use tokio::sync::Mutex as TokioMutex;
        use once_cell::sync::OnceCell;
        use rula_std::qnic::QnicInterface;
        use rula_std::qubit::QubitInterface;
        use async_trait::async_trait;
        use log::warn;
    )
}
