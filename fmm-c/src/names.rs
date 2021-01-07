use fmm::types;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

pub fn generate_argument_name(index: usize) -> String {
    format!("a{}", index)
}

pub fn generate_record_element_name(index: usize) -> String {
    format!("e{}", index)
}

pub fn generate_union_member_name(index: usize) -> String {
    format!("m{}", index)
}

pub fn generate_record_type_name(record: &types::Record) -> String {
    format!("t_{}", hash(record))
}

pub fn generate_union_type_name(union: &types::Union) -> String {
    format!("t_{:x}", hash(union))
}

fn hash(hash: impl Hash) -> u64 {
    let mut hasher = DefaultHasher::new();

    hash.hash(&mut hasher);

    hasher.finish()
}
