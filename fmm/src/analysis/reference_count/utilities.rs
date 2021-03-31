use crate::types;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

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
