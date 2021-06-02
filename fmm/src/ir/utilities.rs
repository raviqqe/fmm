use super::record::Record;
use crate::types::VOID_TYPE;
use once_cell::sync::Lazy;

pub static VOID_VALUE: Lazy<Record> = Lazy::new(|| Record::new(VOID_TYPE.clone(), vec![]));
