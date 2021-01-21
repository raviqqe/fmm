use super::types::*;
use crate::names::*;
use fmm::ir::*;
use fmm::types;

pub fn compile_expression(expression: &Expression) -> String {
    match expression {
        Expression::Primitive(primitive) => compile_primitive(*primitive),
        Expression::Record(record) => {
            format!(
                "({}){{{}}}",
                compile_record_type_id(record.type_()),
                record
                    .elements()
                    .iter()
                    .map(compile_expression)
                    .collect::<Vec<_>>()
                    .join(",")
            )
        }
        Expression::Undefined(undefined) => compile_undefined(undefined),
        Expression::Union(union) => {
            format!(
                "({}){{.{}={}}}",
                compile_union_type_id(union.type_()),
                generate_union_member_name(union.member_index()),
                compile_expression(union.member())
            )
        }
        Expression::Variable(variable) => variable.name().into(),
    }
}

fn compile_undefined(undefined: &Undefined) -> String {
    match undefined.type_() {
        types::Type::Function(_) => "NULL".into(),
        types::Type::Primitive(primitive) => compile_undefined_primitive(*primitive).into(),
        types::Type::Pointer(_) => "NULL".into(),
        types::Type::Record(record) => format!("({}){{}}", compile_record_type_id(record)),
        types::Type::Union(union) => format!("({}){{}}", compile_union_type_id(union)),
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
