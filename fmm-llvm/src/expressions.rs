use super::types::*;
use crate::union::compile_union_cast;
use fmm::{ir::*, types};
use inkwell::values::BasicValue;
use std::collections::HashMap;

pub fn compile_expression<'c>(
    builder: &inkwell::builder::Builder<'c>,
    expression: &Expression,
    variables: &HashMap<String, inkwell::values::BasicValueEnum<'c>>,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::values::BasicValueEnum<'c> {
    let compile_expression = |expression: &Expression| {
        compile_expression(builder, expression, variables, context, target_data)
    };

    match expression {
        Expression::AlignOf(align_of) => compile_align_of(align_of, context, target_data).into(),
        Expression::ArithmeticOperation(operation) => {
            compile_arithmetic_operation(builder, operation, &compile_expression)
        }
        Expression::BitCast(bit_cast) => {
            compile_bit_cast(builder, bit_cast, context, target_data, &compile_expression)
        }
        Expression::BitwiseNotOperation(operation) => {
            compile_bitwise_not_operation(builder, operation, &compile_expression).into()
        }
        Expression::BitwiseOperation(operation) => {
            compile_bitwise_operation(builder, operation, &compile_expression).into()
        }
        Expression::ComparisonOperation(operation) => {
            compile_comparison_operation(builder, operation, &compile_expression)
        }
        Expression::PointerAddress(address) => {
            compile_pointer_address(builder, address, &compile_expression).into()
        }
        Expression::Primitive(primitive) => compile_primitive(*primitive, context, target_data),
        Expression::Record(record) => {
            let mut value = compile_record_type(record.type_(), context, target_data).const_zero();

            for (index, element) in record.elements().iter().enumerate() {
                value = builder
                    .build_insert_value(value, compile_expression(element), index as u32, "")
                    .unwrap()
                    .into_struct_value();
            }

            value.into()
        }
        Expression::RecordAddress(address) => {
            compile_record_address(builder, address, context, &compile_expression).into()
        }
        Expression::SizeOf(size_of) => compile_size_of(size_of, context, target_data).into(),
        Expression::Undefined(undefined) => compile_undefined(undefined, context, target_data),
        Expression::Union(union) => {
            let member = compile_expression(union.member());

            let value = context.const_struct(
                &[
                    member.get_type().const_zero(),
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
            );

            compile_union_cast(
                builder,
                builder
                    .build_insert_value(value, member, 0, "")
                    .unwrap()
                    .as_basic_value_enum(),
                compile_union_type(union.type_(), context, target_data).into(),
            )
        }
        Expression::UnionAddress(address) => {
            compile_union_address(builder, address, context, target_data, &compile_expression)
                .into()
        }
        Expression::Variable(variable) => variables[variable.name()],
    }
}

pub fn compile_constant_expression<'c>(
    expression: &Expression,
    variables: &HashMap<String, inkwell::values::BasicValueEnum<'c>>,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::values::BasicValueEnum<'c> {
    let compile_expression = |expression: &Expression| {
        compile_constant_expression(expression, variables, context, target_data)
    };

    match expression {
        Expression::AlignOf(align_of) => compile_align_of(align_of, context, target_data).into(),
        Expression::ArithmeticOperation(operation) => {
            compile_arithmetic_operation(&context.create_builder(), operation, &compile_expression)
        }
        Expression::BitCast(bit_cast) => compile_constant_bit_cast(
            &context.create_builder(),
            bit_cast,
            context,
            target_data,
            &compile_expression,
        ),
        Expression::BitwiseNotOperation(operation) => {
            compile_bitwise_not_operation(&context.create_builder(), operation, &compile_expression)
                .into()
        }
        Expression::BitwiseOperation(operation) => {
            compile_bitwise_operation(&context.create_builder(), operation, &compile_expression)
                .into()
        }
        Expression::ComparisonOperation(operation) => {
            compile_comparison_operation(&context.create_builder(), operation, &compile_expression)
        }
        Expression::PointerAddress(address) => {
            compile_pointer_address(&context.create_builder(), address, &compile_expression).into()
        }
        Expression::Primitive(primitive) => compile_primitive(*primitive, context, target_data),
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
        Expression::RecordAddress(address) => compile_record_address(
            &context.create_builder(),
            address,
            context,
            &compile_expression,
        )
        .into(),
        Expression::SizeOf(size_of) => compile_size_of(size_of, context, target_data).into(),
        Expression::Undefined(undefined) => compile_undefined(undefined, context, target_data),
        Expression::Union(union) => context
            .const_struct(
                &[
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
        Expression::UnionAddress(address) => compile_union_address(
            &context.create_builder(),
            address,
            context,
            target_data,
            &compile_expression,
        )
        .into(),
        Expression::Variable(variable) => variables[variable.name()],
    }
}

fn compile_align_of<'c>(
    align_of: &AlignOf,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::values::IntValue<'c> {
    compile_pointer_integer(
        target_data.get_abi_alignment(&compile_type(align_of.type_(), context, target_data)) as u64,
        context,
        target_data,
    )
}

fn compile_arithmetic_operation<'c>(
    builder: &inkwell::builder::Builder<'c>,
    operation: &ArithmeticOperation,
    compile_expression: &impl Fn(&Expression) -> inkwell::values::BasicValueEnum<'c>,
) -> inkwell::values::BasicValueEnum<'c> {
    let lhs = compile_expression(operation.lhs());
    let rhs = compile_expression(operation.rhs());

    match operation.type_() {
        fmm::types::Primitive::Boolean
        | fmm::types::Primitive::Integer8
        | fmm::types::Primitive::Integer32
        | fmm::types::Primitive::Integer64
        | fmm::types::Primitive::PointerInteger => {
            let lhs = lhs.into_int_value();
            let rhs = rhs.into_int_value();

            match operation.operator() {
                fmm::ir::ArithmeticOperator::Add => builder.build_int_add(lhs, rhs, ""),
                fmm::ir::ArithmeticOperator::Subtract => builder.build_int_sub(lhs, rhs, ""),
                fmm::ir::ArithmeticOperator::Multiply => builder.build_int_mul(lhs, rhs, ""),
                fmm::ir::ArithmeticOperator::Divide => builder.build_int_unsigned_div(lhs, rhs, ""),
            }
            .into()
        }
        fmm::types::Primitive::Float32 | fmm::types::Primitive::Float64 => {
            let lhs = lhs.into_float_value();
            let rhs = rhs.into_float_value();

            match operation.operator() {
                fmm::ir::ArithmeticOperator::Add => builder.build_float_add(lhs, rhs, ""),
                fmm::ir::ArithmeticOperator::Subtract => builder.build_float_sub(lhs, rhs, ""),
                fmm::ir::ArithmeticOperator::Multiply => builder.build_float_mul(lhs, rhs, ""),
                fmm::ir::ArithmeticOperator::Divide => builder.build_float_div(lhs, rhs, ""),
            }
            .into()
        }
    }
}

fn compile_bit_cast<'c>(
    builder: &inkwell::builder::Builder<'c>,
    bit_cast: &BitCast,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
    compile_expression: &impl Fn(&Expression) -> inkwell::values::BasicValueEnum<'c>,
) -> inkwell::values::BasicValueEnum<'c> {
    if is_constant_bit_cast_supported(bit_cast.from())
        && is_constant_bit_cast_supported(bit_cast.to())
    {
        compile_constant_bit_cast(builder, bit_cast, context, target_data, compile_expression)
    } else {
        let pointer = builder.build_alloca(compile_type(bit_cast.from(), context, target_data), "");

        builder.build_store(pointer, compile_expression(bit_cast.expression()));

        builder.build_load(
            builder
                .build_bitcast(
                    pointer,
                    compile_pointer_type(
                        &types::Pointer::new(bit_cast.to().clone()),
                        context,
                        target_data,
                    ),
                    "",
                )
                .into_pointer_value(),
            "",
        )
    }
}

fn compile_constant_bit_cast<'c>(
    builder: &inkwell::builder::Builder<'c>,
    bit_cast: &BitCast,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
    compile_expression: &impl Fn(&Expression) -> inkwell::values::BasicValueEnum<'c>,
) -> inkwell::values::BasicValueEnum<'c> {
    let argument = compile_expression(bit_cast.expression());
    let to_type = compile_type(bit_cast.to(), context, target_data);

    let value = builder.build_bitcast(
        if argument.is_pointer_value() {
            builder
                .build_ptr_to_int(
                    argument.into_pointer_value(),
                    compile_pointer_integer_type(context, target_data),
                    "",
                )
                .into()
        } else {
            argument
        },
        if to_type.is_pointer_type() {
            compile_pointer_integer_type(context, target_data).into()
        } else {
            to_type
        },
        "",
    );

    if to_type.is_pointer_type() {
        builder
            .build_int_to_ptr(value.into_int_value(), to_type.into_pointer_type(), "")
            .into()
    } else {
        value
    }
}

fn is_constant_bit_cast_supported(type_: &fmm::types::Type) -> bool {
    matches!(
        type_,
        fmm::types::Type::Function(_)
            | fmm::types::Type::Pointer(_)
            | fmm::types::Type::Primitive(_)
    )
}

fn compile_bitwise_not_operation<'c>(
    builder: &inkwell::builder::Builder<'c>,
    operation: &BitwiseNotOperation,
    compile_expression: &impl Fn(&Expression) -> inkwell::values::BasicValueEnum<'c>,
) -> inkwell::values::IntValue<'c> {
    builder.build_not(compile_expression(operation.value()).into_int_value(), "")
}

fn compile_bitwise_operation<'c>(
    builder: &inkwell::builder::Builder<'c>,
    operation: &BitwiseOperation,
    compile_expression: &impl Fn(&Expression) -> inkwell::values::BasicValueEnum<'c>,
) -> inkwell::values::IntValue<'c> {
    let lhs = compile_expression(operation.lhs()).into_int_value();
    let rhs = compile_expression(operation.rhs()).into_int_value();

    match operation.operator() {
        fmm::ir::BitwiseOperator::And => builder.build_and(lhs, rhs, ""),
        fmm::ir::BitwiseOperator::Or => builder.build_or(lhs, rhs, ""),
        fmm::ir::BitwiseOperator::Xor => builder.build_xor(lhs, rhs, ""),
    }
}

fn compile_comparison_operation<'c>(
    builder: &inkwell::builder::Builder<'c>,
    operation: &ComparisonOperation,
    compile_expression: &impl Fn(&Expression) -> inkwell::values::BasicValueEnum<'c>,
) -> inkwell::values::BasicValueEnum<'c> {
    match operation.type_() {
        fmm::types::Primitive::Boolean
        | fmm::types::Primitive::Integer8
        | fmm::types::Primitive::Integer32
        | fmm::types::Primitive::Integer64
        | fmm::types::Primitive::PointerInteger => builder.build_int_compare(
            compile_integer_comparison_operator(operation.operator()),
            compile_expression(operation.lhs()).into_int_value(),
            compile_expression(operation.rhs()).into_int_value(),
            "",
        ),
        fmm::types::Primitive::Float32 | fmm::types::Primitive::Float64 => builder
            .build_float_compare(
                compile_float_comparison_operator(operation.operator()),
                compile_expression(operation.lhs()).into_float_value(),
                compile_expression(operation.rhs()).into_float_value(),
                "",
            ),
    }
    .into()
}

fn compile_record_address<'c>(
    builder: &inkwell::builder::Builder<'c>,
    address: &RecordAddress,
    context: &'c inkwell::context::Context,
    compile_expression: &impl Fn(&Expression) -> inkwell::values::BasicValueEnum<'c>,
) -> inkwell::values::PointerValue<'c> {
    unsafe {
        builder.build_gep(
            compile_expression(address.pointer()).into_pointer_value(),
            &[
                context.i32_type().const_zero(),
                context
                    .i32_type()
                    .const_int(address.element_index() as u64, false),
            ],
            "",
        )
    }
}

fn compile_size_of<'c>(
    size_of: &SizeOf,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::values::IntValue<'c> {
    compile_pointer_integer(
        target_data.get_store_size(&compile_type(size_of.type_(), context, target_data)) as u64,
        context,
        target_data,
    )
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
            compile_undefined_primitive(*primitive, context, target_data)
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
    target_data: &inkwell::targets::TargetData,
) -> inkwell::values::BasicValueEnum<'c> {
    match primitive {
        Primitive::Boolean(boolean) => context.bool_type().const_int(boolean as u64, false).into(),
        Primitive::Float32(number) => context.f32_type().const_float(number as f64).into(),
        Primitive::Float64(number) => context.f64_type().const_float(number as f64).into(),
        Primitive::Integer8(number) => context.i8_type().const_int(number as u64, false).into(),
        Primitive::Integer32(number) => context.i32_type().const_int(number as u64, false).into(),
        Primitive::Integer64(number) => context.i64_type().const_int(number, false).into(),
        Primitive::PointerInteger(number) => {
            compile_pointer_integer(number as u64, context, target_data).into()
        }
    }
}

pub fn compile_pointer_integer<'c>(
    number: u64,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::values::IntValue<'c> {
    compile_pointer_integer_type(context, target_data).const_int(number, false)
}

fn compile_pointer_address<'c>(
    builder: &inkwell::builder::Builder<'c>,
    address: &PointerAddress,
    compile_expression: &impl Fn(&Expression) -> inkwell::values::BasicValueEnum<'c>,
) -> inkwell::values::PointerValue<'c> {
    unsafe {
        builder.build_gep(
            compile_expression(address.pointer()).into_pointer_value(),
            &[compile_expression(address.offset()).into_int_value()],
            "",
        )
    }
}

// TODO Refactor this by matching with types::Primitive directly.
fn compile_undefined_primitive<'c>(
    type_: types::Primitive,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::values::BasicValueEnum<'c> {
    match compile_primitive_type(type_, context, target_data) {
        inkwell::types::BasicTypeEnum::FloatType(float) => float.const_zero().into(),
        inkwell::types::BasicTypeEnum::IntType(integer) => integer.const_zero().into(),
        inkwell::types::BasicTypeEnum::PointerType(pointer) => pointer.const_zero().into(),
        _ => unreachable!(),
    }
}

fn compile_union_address<'c>(
    builder: &inkwell::builder::Builder<'c>,
    address: &UnionAddress,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
    compile_expression: &impl Fn(&Expression) -> inkwell::values::BasicValueEnum<'c>,
) -> inkwell::values::PointerValue<'c> {
    unsafe {
        builder.build_gep(
            builder
                .build_bitcast(
                    compile_expression(address.pointer()),
                    compile_union_member_type(
                        address.type_(),
                        address.member_index(),
                        context,
                        target_data,
                    )
                    .ptr_type(DEFAULT_ADDRESS_SPACE),
                    "",
                )
                .into_pointer_value(),
            &[
                context.i32_type().const_zero(),
                context.i32_type().const_zero(),
            ],
            "",
        )
    }
}

fn compile_integer_comparison_operator(operator: ComparisonOperator) -> inkwell::IntPredicate {
    match operator {
        ComparisonOperator::Equal => inkwell::IntPredicate::EQ,
        ComparisonOperator::NotEqual => inkwell::IntPredicate::NE,
        ComparisonOperator::LessThan => inkwell::IntPredicate::ULT,
        ComparisonOperator::LessThanOrEqual => inkwell::IntPredicate::ULE,
        ComparisonOperator::GreaterThan => inkwell::IntPredicate::UGT,
        ComparisonOperator::GreaterThanOrEqual => inkwell::IntPredicate::UGE,
    }
}

fn compile_float_comparison_operator(operator: ComparisonOperator) -> inkwell::FloatPredicate {
    match operator {
        ComparisonOperator::Equal => inkwell::FloatPredicate::OEQ,
        ComparisonOperator::NotEqual => inkwell::FloatPredicate::ONE,
        ComparisonOperator::LessThan => inkwell::FloatPredicate::OLT,
        ComparisonOperator::LessThanOrEqual => inkwell::FloatPredicate::OLE,
        ComparisonOperator::GreaterThan => inkwell::FloatPredicate::OGT,
        ComparisonOperator::GreaterThanOrEqual => inkwell::FloatPredicate::OGE,
    }
}
