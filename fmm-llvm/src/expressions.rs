use super::types::*;
use fmm::ir::*;
use fmm::types;
use std::collections::HashMap;

pub fn compile_expression<'c>(
    expression: &Expression,
    variables: &HashMap<String, inkwell::values::BasicValueEnum<'c>>,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::values::BasicValueEnum<'c> {
    let compile_expression =
        |expression| compile_expression(expression, variables, context, target_data);
    let compile_type = |type_| compile_type(type_, context, target_data);

    match expression {
        Expression::AlignOf(align_of) => compile_pointer_integer(
            target_data.get_abi_alignment(&compile_type(align_of.type_())) as u64,
            context,
        )
        .into(),
        Expression::BitCast(bit_cast) => context.create_builder().build_bitcast(
            compile_expression(bit_cast.expression()),
            compile_type(bit_cast.to()),
            "",
        ),
        Expression::Primitive(primitive) => compile_primitive(*primitive, context),
        Expression::Record(record) => context
            .const_struct(
                &record
                    .elements()
                    .iter()
                    .map(|expression| compile_expression(expression))
                    .collect::<Vec<_>>(),
                false,
            )
            .into(),
        Expression::SizeOf(size_of) => compile_pointer_integer(
            target_data.get_store_size(&compile_type(size_of.type_())) as u64,
            context,
        )
        .into(),
        Expression::Undefined(undefined) => compile_undefined(undefined, context, target_data),
        Expression::Union(union) => context
            .const_struct(
                &vec![
                    compile_expression(union.member()),
                    compile_union_member_padding_type(
                        union.type_(),
                        union.member_index(),
                        context,
                        target_data,
                    )
                    .const_zero()
                    .into(),
                ],
                false,
            )
            .into(),
        Expression::Variable(variable) => variables[variable.name()],
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
        Primitive::Boolean(boolean) => context.bool_type().const_int(boolean as u64, false).into(),
        Primitive::Float32(number) => context.f32_type().const_float(number as f64).into(),
        Primitive::Float64(number) => context.f64_type().const_float(number as f64).into(),
        Primitive::Integer8(number) => context.i8_type().const_int(number as u64, false).into(),
        Primitive::Integer32(number) => context.i32_type().const_int(number as u64, false).into(),
        Primitive::Integer64(number) => context.i64_type().const_int(number, false).into(),
        Primitive::PointerInteger(number) => {
            compile_pointer_integer(number as u64, context).into()
        }
    }
}

pub fn compile_pointer_integer<'c>(
    number: u64,
    context: &'c inkwell::context::Context,
) -> inkwell::values::PointerValue<'c> {
    context
        .i64_type()
        .const_int(number, false)
        .const_to_pointer(compile_pointer_integer_type(context))
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
