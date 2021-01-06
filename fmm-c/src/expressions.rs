use fmm::ir::*;
use fmm::types::{self, Type};

pub fn compile_expression(expression: &Expression) -> String {
    match expression {
        Expression::Primitive(_) => todo!(),
        Expression::Record(_) => todo!(),
        Expression::Undefined(undefined) => compile_undefined(undefined).into(),
        Expression::Union(_) => todo!(),
        Expression::Variable(_) => todo!(),
    }
}

fn compile_undefined(undefined: &Undefined) -> &str {
    match undefined.type_() {
        Type::Function(_) => "NULL",
        Type::Primitive(primitive) => compile_undefined_primitive(*primitive),
        Type::Pointer(_) => "NULL",
        Type::Record(_) => "{}",
        Type::Union(_) => "{}",
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
