use crate::{ir::Record, types};
use once_cell::sync::Lazy;

pub static VOID_TYPE: Lazy<types::Record> = Lazy::new(|| types::Record::new(vec![]));
pub static VOID_VALUE: Lazy<Record> = Lazy::new(|| Record::new(VOID_TYPE.clone(), vec![]));
