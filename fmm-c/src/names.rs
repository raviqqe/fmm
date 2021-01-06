pub fn generate_argument_name(index: usize) -> String {
    format!("a{}", index)
}

pub fn generate_record_element_name(index: usize) -> String {
    format!("e{}", index)
}

pub fn generate_union_member_name(index: usize) -> String {
    format!("m{}", index)
}
