use super::types::*;
use crate::names::*;
use fmm::ir::*;
use fmm::types;
use std::collections::{HashMap, HashSet};

pub fn compile_expression(
    expression: &Expression,
    global_variables: &HashSet<String>,
    type_ids: &HashMap<fmm::types::Type, String>,
) -> String {
    let compile_expression =
        |expression| compile_expression(expression, global_variables, type_ids);

    match expression {
        Expression::AlignOf(align_of) => {
            format!("alignof({})", compile_type_id(align_of.type_(), type_ids))
        }
        Expression::Primitive(primitive) => compile_primitive(*primitive),
        Expression::Record(record) => {
            format!(
                "({}){{{}}}",
                compile_record_type_id(record.type_(), type_ids),
                record
                    .elements()
                    .iter()
                    .map(|expression| compile_expression(expression))
                    .collect::<Vec<_>>()
                    .join(",")
            )
        }
        Expression::SizeOf(size_of) => {
            format!("sizeof({})", compile_type_id(size_of.type_(), type_ids))
        }
        Expression::Undefined(undefined) => compile_undefined(undefined, type_ids),
        Expression::Union(union) => {
            format!(
                "({}){{.{}={}}}",
                compile_union_type_id(union.type_(), type_ids),
                generate_union_member_name(union.member_index()),
                compile_expression(union.member())
            )
        }
        Expression::Variable(variable) => {
            if global_variables.contains(variable.name()) {
                "&"
            } else {
                ""
            }
            .to_owned()
                + variable.name()
        }
    }
}

fn compile_undefined(
    undefined: &Undefined,
    type_ids: &HashMap<fmm::types::Type, String>,
) -> String {
    match undefined.type_() {
        types::Type::Function(_) => "NULL".into(),
        types::Type::Primitive(primitive) => compile_undefined_primitive(*primitive).into(),
        types::Type::Pointer(_) => "NULL".into(),
        types::Type::Record(record) => {
            format!("({}){{}}", compile_record_type_id(record, type_ids))
        }
        types::Type::Union(union) => format!("({}){{}}", compile_union_type_id(union, type_ids)),
    }
}

fn compile_primitive(primitive: Primitive) -> String {
    match primitive {
        Primitive::Boolean(boolean) => format!("{}", boolean),
        Primitive::Float32(number) => format!("{}", number),
        Primitive::Float64(number) => format!("{}", number),
        Primitive::Integer8(number) => format!("{}", number),
        Primitive::Integer32(number) => format!("{}", number),
        Primitive::Integer64(number) => format!("{}", number),
        Primitive::PointerInteger(number) => format!("{}", number),
    }
}

fn compile_undefined_primitive(primitive: types::Primitive) -> &'static str {
    match primitive {
        types::Primitive::Boolean => "false",
        types::Primitive::Float32 | types::Primitive::Float64 => "0.0",
        types::Primitive::Integer8
        | types::Primitive::Integer32
        | types::Primitive::Integer64
        | types::Primitive::PointerInteger => "0",
    }
}
