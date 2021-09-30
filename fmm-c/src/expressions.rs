use super::types::*;
use crate::names::*;
use fmm::{ir::*, types};
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
        Expression::ArithmeticOperation(operation) => format!(
            "{}{}{}",
            compile_expression(operation.lhs()),
            compile_arithmetic_operator(operation.operator()),
            compile_expression(operation.rhs()),
        ),
        Expression::BitCast(bit_cast) => {
            format!(
                "__builtin_bit_cast({},({})({}))",
                compile_type_id(bit_cast.to(), type_ids),
                compile_type_id(bit_cast.from(), type_ids),
                compile_expression(bit_cast.expression()),
            )
        }
        Expression::BitwiseNotOperation(operation) => {
            format!("(~({}))", compile_expression(operation.value()))
        }
        Expression::BitwiseOperation(operation) => {
            format!(
                "(({}){}({}))",
                compile_expression(operation.lhs()),
                match operation.operator() {
                    fmm::ir::BitwiseOperator::And => "&",
                    fmm::ir::BitwiseOperator::Or => "|",
                    fmm::ir::BitwiseOperator::Xor => "^",
                },
                compile_expression(operation.rhs()),
            )
        }
        Expression::ComparisonOperation(operation) => format!(
            "{}{}{}",
            compile_expression(operation.lhs()),
            compile_comparison_operator(operation.operator()),
            compile_expression(operation.rhs()),
        ),
        Expression::PointerAddress(address) => format!(
            "(({})+({}))",
            compile_expression(address.pointer()),
            compile_expression(address.offset()),
        ),
        Expression::Primitive(primitive) => compile_primitive(*primitive),
        Expression::Record(record) => {
            format!(
                "({}){{{}}}",
                compile_record_type_id(record.type_(), type_ids),
                record
                    .elements()
                    .iter()
                    .map(compile_expression)
                    .collect::<Vec<_>>()
                    .join(",")
            )
        }
        Expression::RecordAddress(address) => format!(
            "(&({})->{})",
            compile_expression(address.pointer()),
            generate_record_element_name(address.element_index()),
        ),
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
        Expression::UnionAddress(address) => {
            format!(
                "(&({})->{})",
                compile_expression(address.pointer()),
                generate_union_member_name(address.member_index()),
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
        types::Type::Pointer(_) => {
            format!("({})NULL", compile_type_id(undefined.type_(), type_ids))
        }
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

fn compile_arithmetic_operator(operator: ArithmeticOperator) -> &'static str {
    match operator {
        ArithmeticOperator::Add => "+",
        ArithmeticOperator::Subtract => "-",
        ArithmeticOperator::Multiply => "*",
        ArithmeticOperator::Divide => "/",
    }
}

fn compile_comparison_operator(operator: ComparisonOperator) -> &'static str {
    match operator {
        ComparisonOperator::Equal => "==",
        ComparisonOperator::NotEqual => "!=",
        ComparisonOperator::LessThan => "<",
        ComparisonOperator::LessThanOrEqual => "<=",
        ComparisonOperator::GreaterThan => ">",
        ComparisonOperator::GreaterThanOrEqual => ">=",
    }
}
