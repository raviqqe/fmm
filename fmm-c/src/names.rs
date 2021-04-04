pub fn generate_argument_name(index: usize) -> String {
    format!("a{}", index)
}

pub fn generate_record_element_name(index: usize) -> String {
    format!("e{}", index)
}

pub fn generate_union_member_name(index: usize) -> String {
    format!("m{}", index)
}

pub fn generate_record_type_name(index: usize) -> String {
    format!("r{}", index)
}

pub fn generate_union_type_name(index: usize) -> String {
    format!("u{}", index)
}

pub fn generate_tagged_union_type_name(index: usize) -> String {
    format!("t{}", index)
}
