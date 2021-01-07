use crate::names::*;
use fmm::ir::*;
use fmm::types::{self, Type};

pub fn compile_expression(expression: &Expression) -> String {
    match expression {
        Expression::Primitive(primitive) => compile_primitive(*primitive),
        Expression::Record(record) => {
            format!(
                "(struct {}){{{}}}",
                generate_record_type_name(record.type_()),
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
                "(union {}){{.{} = {}}}",
                generate_union_type_name(union.type_()),
                generate_union_member_name(union.member_index()),
                compile_expression(union.member())
            )
        }
        Expression::Variable(variable) => variable.name().into(),
    }
}

fn compile_undefined(undefined: &Undefined) -> String {
    match undefined.type_() {
        Type::Function(_) => "NULL".into(),
        Type::Primitive(primitive) => compile_undefined_primitive(*primitive).into(),
        Type::Pointer(_) => "NULL".into(),
        Type::Record(record) => format!("(struct {}){{}}", generate_record_type_name(record)),
        Type::Union(union) => format!("(union {}){{}}", generate_union_type_name(union)),
    }
}

fn compile_primitive(primitive: Primitive) -> String {
    match primitive {
        Primitive::Bool(bool) => format!("{}", bool),
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
        types::Primitive::Bool => "false",
        types::Primitive::Float32 | types::Primitive::Float64 => "0.0",
        types::Primitive::Integer8
        | types::Primitive::Integer32
        | types::Primitive::Integer64
        | types::Primitive::PointerInteger => "0",
    }
}
