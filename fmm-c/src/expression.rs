use crate::{name, type_};
use fmm::{ir::*, types};
use fnv::{FnvHashMap, FnvHashSet};

pub fn compile(
    expression: &Expression,
    global_variables: &FnvHashSet<String>,
    type_ids: &FnvHashMap<fmm::types::Type, String>,
) -> String {
    let compile = |expression| compile(expression, global_variables, type_ids);

    match expression {
        Expression::AlignOf(align_of) => {
            format!("alignof({})", type_::compile_id(align_of.type_(), type_ids))
        }
        Expression::ArithmeticOperation(operation) => format!(
            "{}{}{}",
            compile(operation.lhs()),
            compile_arithmetic_operator(operation.operator()),
            compile(operation.rhs()),
        ),
        Expression::BitCast(bit_cast) => {
            format!(
                "__builtin_bit_cast({},({})({}))",
                type_::compile_id(bit_cast.to(), type_ids),
                type_::compile_id(bit_cast.from(), type_ids),
                compile(bit_cast.expression()),
            )
        }
        Expression::BitwiseNotOperation(operation) => {
            format!("(~({}))", compile(operation.value()))
        }
        Expression::BitwiseOperation(operation) => {
            let lhs = compile(operation.lhs());
            let rhs = compile(operation.rhs());
            let compile_unsigned = |operator| format!("(({}){}({}))", lhs, operator, rhs);

            match operation.operator() {
                fmm::ir::BitwiseOperator::And => compile_unsigned("&"),
                fmm::ir::BitwiseOperator::Or => compile_unsigned("|"),
                fmm::ir::BitwiseOperator::Xor => compile_unsigned("^"),
                fmm::ir::BitwiseOperator::LeftShift => compile_unsigned("<<"),
                fmm::ir::BitwiseOperator::RightShift(signed) => {
                    if signed {
                        unimplemented!()
                    } else {
                        compile_unsigned(">>")
                    }
                }
            }
        }
        Expression::ComparisonOperation(operation) => {
            let lhs = compile(operation.lhs());
            let rhs = compile(operation.rhs());
            let compile_unsigned = |operator| format!("(({}){}({}))", lhs, operator, rhs);

            match operation.operator() {
                ComparisonOperator::Equal => compile_unsigned("=="),
                ComparisonOperator::NotEqual => compile_unsigned("!="),
                ComparisonOperator::LessThan(signed) => {
                    if signed {
                        unimplemented!()
                    } else {
                        compile_unsigned("<")
                    }
                }
                ComparisonOperator::LessThanOrEqual(signed) => {
                    if signed {
                        unimplemented!()
                    } else {
                        compile_unsigned("<=")
                    }
                }
                ComparisonOperator::GreaterThan(signed) => {
                    if signed {
                        unimplemented!()
                    } else {
                        compile_unsigned(">")
                    }
                }
                ComparisonOperator::GreaterThanOrEqual(signed) => {
                    if signed {
                        unimplemented!()
                    } else {
                        compile_unsigned(">=")
                    }
                }
            }
        }
        Expression::PointerAddress(address) => format!(
            "(({})+({}))",
            compile(address.pointer()),
            compile(address.offset()),
        ),
        Expression::Primitive(primitive) => compile_primitive(*primitive),
        Expression::Record(record) => {
            format!(
                "({}){{{}}}",
                type_::compile_record_id(record.type_(), type_ids),
                record
                    .fields()
                    .iter()
                    .map(compile)
                    .collect::<Vec<_>>()
                    .join(",")
            )
        }
        Expression::RecordAddress(address) => format!(
            "(&({})->{})",
            compile(address.pointer()),
            name::generate_record_field_name(address.field_index()),
        ),
        Expression::SizeOf(size_of) => {
            format!("sizeof({})", type_::compile_id(size_of.type_(), type_ids))
        }
        Expression::Undefined(undefined) => compile_undefined(undefined, type_ids),
        Expression::Union(union) => {
            format!(
                "({}){{.{}={}}}",
                type_::compile_union_id(union.type_(), type_ids),
                name::generate_union_member_name(union.member_index()),
                compile(union.member())
            )
        }
        Expression::UnionAddress(address) => {
            format!(
                "(&({})->{})",
                compile(address.pointer()),
                name::generate_union_member_name(address.member_index()),
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
    type_ids: &FnvHashMap<fmm::types::Type, String>,
) -> String {
    match undefined.type_() {
        types::Type::Function(_) => "NULL".into(),
        types::Type::Primitive(primitive) => compile_undefined_primitive(*primitive).into(),
        types::Type::Pointer(_) => {
            format!("({})NULL", type_::compile_id(undefined.type_(), type_ids))
        }
        types::Type::Record(record) => {
            format!("({}){{}}", type_::compile_record_id(record, type_ids))
        }
        types::Type::Union(union) => {
            format!("({}){{}}", type_::compile_union_id(union, type_ids))
        }
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
