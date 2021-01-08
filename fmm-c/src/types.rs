use super::names::*;
use fmm::types::{self, Type};

pub fn compile_typed_name(type_: &Type, name: &str) -> String {
    match type_ {
        Type::Function(function) => compile_function_name(function, &format!("(*{})", name)),
        Type::Primitive(primitive) => compile_primitive_type_id(*primitive) + " " + name,
        Type::Record(record) => compile_record_type_id(record) + " " + name,
        Type::Pointer(pointer) => compile_typed_name(pointer.element(), &format!("*{}", name)),
        Type::Union(union) => compile_union_type_id(union) + " " + name,
    }
}

pub fn compile_function_name(function: &types::Function, name: &str) -> String {
    compile_typed_name(
        function.result(),
        &format!(
            "{}({})",
            name,
            function
                .arguments()
                .iter()
                .enumerate()
                .map(|(index, type_)| compile_typed_name(type_, &generate_argument_name(index)))
                .collect::<Vec<_>>()
                .join(",")
        ),
    )
}

pub fn compile_type_id(type_: &Type) -> String {
    match type_ {
        Type::Function(function) => compile_function_name(function, "(*)"),
        Type::Primitive(primitive) => compile_primitive_type_id(*primitive),
        Type::Record(record) => compile_record_type_id(record),
        Type::Pointer(pointer) => compile_typed_name(pointer.element(), "*"),
        Type::Union(union) => compile_union_type_id(union),
    }
}

pub fn compile_atomic_pointer_type_id(type_: &Type) -> String {
    match type_ {
        Type::Function(function) => compile_function_name(function, "(*_Atomic *)"),
        Type::Pointer(pointer) => compile_typed_name(pointer.element(), "_Atomic *"),
        Type::Primitive(_) | Type::Record(_) | Type::Union(_) => {
            "_Atomic ".to_owned() + &compile_type_id(type_) + " *"
        }
    }
}

pub fn compile_primitive_type_id(primitive: types::Primitive) -> String {
    match primitive {
        types::Primitive::Bool => "bool",
        types::Primitive::Float32 => "float",
        types::Primitive::Float64 => "double",
        types::Primitive::Integer8 => "uint8_t",
        types::Primitive::Integer32 => "uint32_t",
        types::Primitive::Integer64 => "uint64_t",
        types::Primitive::PointerInteger => "size_t",
    }
    .into()
}

pub fn compile_record_type_id(record: &types::Record) -> String {
    "struct ".to_owned() + &generate_record_type_name(record)
}

pub fn compile_union_type_id(union: &types::Union) -> String {
    "union ".to_owned() + &generate_union_type_name(union)
}

pub fn compile_record_elements(record: &types::Record) -> String {
    record
        .elements()
        .iter()
        .enumerate()
        .map(|(index, type_)| compile_typed_name(type_, &generate_record_element_name(index)) + ";")
        .collect::<Vec<_>>()
        .join("")
}

pub fn compile_union_members(union: &types::Union) -> String {
    union
        .members()
        .iter()
        .enumerate()
        .map(|(index, type_)| compile_typed_name(type_, &generate_union_member_name(index)) + ";")
        .collect::<Vec<_>>()
        .join("")
}
