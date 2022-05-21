use crate::name::*;
use fmm::types::{self, Type};
use fnv::FnvHashMap;

pub fn compile_name(type_: &Type, name: &str, type_ids: &FnvHashMap<Type, String>) -> String {
    match type_ {
        Type::Function(function) => {
            compile_function_name(function, &format!("(*{})", name), type_ids)
        }
        Type::Primitive(primitive) => compile_primitive_id(*primitive) + " " + name,
        Type::Record(record) => compile_record_id(record, type_ids) + " " + name,
        Type::Pointer(pointer) => compile_name(pointer.element(), &format!("*{}", name), type_ids),
        Type::Union(union) => compile_union_id(union, type_ids) + " " + name,
    }
}

pub fn compile_function_name(
    function: &types::Function,
    name: &str,
    type_ids: &FnvHashMap<Type, String>,
) -> String {
    compile_name(
        function.result(),
        &format!(
            "{}({})",
            name,
            function
                .arguments()
                .iter()
                .enumerate()
                .map(|(index, type_)| compile_name(type_, &generate_argument_name(index), type_ids))
                .collect::<Vec<_>>()
                .join(",")
        ),
        type_ids,
    )
}

pub fn compile_id(type_: &Type, type_ids: &FnvHashMap<Type, String>) -> String {
    match type_ {
        Type::Function(function) => compile_function_name(function, "(*)", type_ids),
        Type::Primitive(primitive) => compile_primitive_id(*primitive),
        Type::Record(record) => compile_record_id(record, type_ids),
        Type::Pointer(pointer) => compile_name(pointer.element(), "*", type_ids),
        Type::Union(union) => compile_union_id(union, type_ids),
    }
}

pub fn compile_atomic_pointer_id(type_: &Type, type_ids: &FnvHashMap<Type, String>) -> String {
    match type_ {
        Type::Function(function) => compile_function_name(function, "(*_Atomic *)", type_ids),
        Type::Pointer(pointer) => compile_name(pointer.element(), "_Atomic *", type_ids),
        Type::Primitive(_) | Type::Record(_) | Type::Union(_) => {
            "_Atomic ".to_owned() + &compile_id(type_, type_ids) + " *"
        }
    }
}

pub fn compile_primitive_id(primitive: types::Primitive) -> String {
    match primitive {
        types::Primitive::Boolean => "bool",
        types::Primitive::Float32 => "float",
        types::Primitive::Float64 => "double",
        types::Primitive::Integer8 => "uint8_t",
        types::Primitive::Integer32 => "uint32_t",
        types::Primitive::Integer64 => "uint64_t",
        types::Primitive::PointerInteger => "size_t",
    }
    .into()
}

pub fn compile_record_id(record: &types::Record, type_ids: &FnvHashMap<Type, String>) -> String {
    "struct ".to_owned() + &type_ids[&record.clone().into()]
}

pub fn compile_union_id(union: &types::Union, type_ids: &FnvHashMap<Type, String>) -> String {
    "union ".to_owned() + &type_ids[&union.clone().into()]
}

pub fn compile_record_fields(
    record: &types::Record,
    type_ids: &FnvHashMap<Type, String>,
) -> String {
    record
        .fields()
        .iter()
        .enumerate()
        .map(|(index, type_)| {
            compile_name(type_, &generate_record_field_name(index), type_ids) + ";"
        })
        .collect::<Vec<_>>()
        .join("")
}

pub fn compile_union_members(union: &types::Union, type_ids: &FnvHashMap<Type, String>) -> String {
    union
        .members()
        .iter()
        .enumerate()
        .map(|(index, type_)| {
            compile_name(type_, &generate_union_member_name(index), type_ids) + ";"
        })
        .collect::<Vec<_>>()
        .join("")
}
