use super::types::*;
use fmm::ir::*;
use fmm::types;

pub fn compile_expression<'c>(
    expression: &Expression,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::values::BasicValueEnum<'c> {
    let compile_expression = |expression| compile_expression(expression, context, target_data);
    let compile_type = |type_| compile_type(type_, context, target_data);

    match expression {
        Expression::AlignOf(align_of) => {
            target_data.get_abi_alignment(&compile_type(align_of.type_()))
        }
        Expression::BitCast(bit_cast) => {
            format!(
                "__builtin_bit_cast({},({})({}))",
                compile_type_id(bit_cast.to(), type_ids),
                compile_type_id(bit_cast.from(), type_ids),
                compile_expression(bit_cast.expression()),
            )
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

fn compile_undefined<'c>(
    undefined: &Undefined,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::values::BasicValueEnum<'c> {
    match undefined.type_() {
        types::Type::Function(function) => {
            compile_function_pointer_type(function, context, target_data)
                .const_zero()
                .into()
        }
        types::Type::Primitive(primitive) => {
            compile_undefined_primitive(*primitive, context).into()
        }
        types::Type::Pointer(pointer) => compile_pointer_type(pointer, context, target_data)
            .const_zero()
            .into(),
        types::Type::Record(record) => compile_record_type(record, context, target_data)
            .const_zero()
            .into(),
        types::Type::Union(union) => compile_union_type(union, context, target_data)
            .const_zero()
            .into(),
    }
}

// TODO Is signed options for the const_int method correct?
fn compile_primitive<'c>(
    primitive: Primitive,
    context: &'c inkwell::context::Context,
) -> inkwell::values::BasicValueEnum<'c> {
    match primitive {
        Primitive::Boolean(boolean) => context
            .custom_width_int_type(1)
            .const_int(boolean as u64, false)
            .into(),
        Primitive::Float32(number) => context.f32_type().const_float(number as f64).into(),
        Primitive::Float64(number) => context.f64_type().const_float(number as f64).into(),
        Primitive::Integer8(number) => context.i8_type().const_int(number as u64, false).into(),
        Primitive::Integer32(number) => context.i32_type().const_int(number as u64, false).into(),
        Primitive::Integer64(number) => context.i64_type().const_int(number, false).into(),
        Primitive::PointerInteger(number) => {
            context.i64_type().const_int(number as u64, false).into()
        }
    }
}

// TODO Refactor this by matching with types::Primitive directly.
fn compile_undefined_primitive<'c>(
    type_: types::Primitive,
    context: &'c inkwell::context::Context,
) -> inkwell::values::BasicValueEnum<'c> {
    match compile_primitive_type(type_, context) {
        inkwell::types::BasicTypeEnum::FloatType(float) => float.const_zero().into(),
        inkwell::types::BasicTypeEnum::IntType(integer) => integer.const_zero().into(),
        inkwell::types::BasicTypeEnum::PointerType(pointer) => pointer.const_zero().into(),
        _ => unreachable!(),
    }
}
