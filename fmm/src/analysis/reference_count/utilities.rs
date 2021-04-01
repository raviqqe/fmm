use crate::{ir::*, types};
use once_cell::sync::Lazy;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

pub static VOID_TYPE: Lazy<types::Record> = Lazy::new(|| types::Record::new(vec![]));
pub static VOID_VALUE: Lazy<Record> = Lazy::new(|| Record::new(VOID_TYPE.clone(), vec![]));

pub fn get_record_clone_function_name(record: &types::Record) -> String {
    format!("rc_record_clone_{:x}", hash_record_type(record))
}

pub fn get_record_drop_function_name(record: &types::Record) -> String {
    format!("rc_record_drop_{:x}", hash_record_type(record))
}

fn hash_record_type(record: &types::Record) -> u64 {
    let mut hasher = DefaultHasher::new();

    record.hash(&mut hasher);

    hasher.finish()
}
