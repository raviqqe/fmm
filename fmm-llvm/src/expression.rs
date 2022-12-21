use super::type_;
use crate::{context::Context, union::compile_union_cast};
use fmm::ir::*;
use fnv::FnvHashMap;
use inkwell::values::BasicValue;

pub fn compile<'c>(
    context: &Context<'c>,
    builder: &inkwell::builder::Builder<'c>,
    expression: &Expression,
    variables: &FnvHashMap<&str, inkwell::values::BasicValueEnum<'c>>,
) -> inkwell::values::BasicValueEnum<'c> {
    let compile = |expression: &_| compile(context, builder, expression, variables);

    match expression {
        Expression::AlignOf(align_of) => compile_align_of(context, align_of).into(),
        Expression::ArithmeticOperation(operation) => {
            compile_arithmetic_operation(builder, operation, &compile)
        }
        Expression::BitCast(bit_cast) => compile_bit_cast(context, builder, bit_cast, &compile),
        Expression::BitwiseNotOperation(operation) => {
            compile_bitwise_not_operation(builder, operation, &compile).into()
        }
        Expression::BitwiseOperation(operation) => {
            compile_bitwise_operation(builder, operation, &compile).into()
        }
        Expression::ComparisonOperation(operation) => {
            compile_comparison_operation(builder, operation, &compile)
        }
        Expression::PointerAddress(address) => {
            compile_pointer_address(builder, address, &compile).into()
        }
        Expression::Primitive(primitive) => compile_primitive(context, *primitive),
        Expression::Record(record) => {
            let mut value = type_::compile_record(context, record.type_()).const_zero();

            for (index, field) in record.fields().iter().enumerate() {
                value = builder
                    .build_insert_value(value, compile(field), index as u32, "")
                    .unwrap()
                    .into_struct_value();
            }

            value.into()
        }
        Expression::RecordAddress(address) => {
            compile_record_address(context, builder, address, &compile).into()
        }
        Expression::SizeOf(size_of) => compile_size_of(context, size_of).into(),
        Expression::Undefined(undefined) => compile_undefined(context, undefined),
        Expression::Union(union) => {
            let member = compile(union.member());

            let value = context.inkwell().const_struct(
                &[
                    member.get_type().const_zero(),
                    type_::compile_union_member_padding(
                        context,
                        union.type_(),
                        union.member_index(),
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
                type_::compile_union(context, union.type_()).into(),
            )
        }
        Expression::UnionAddress(address) => {
            compile_union_address(context, builder, address, &compile).into()
        }
        Expression::Variable(variable) => variables[variable.name()],
    }
}

pub fn compile_constant<'c>(
    context: &Context<'c>,
    expression: &Expression,
    variables: &FnvHashMap<&str, inkwell::values::BasicValueEnum<'c>>,
) -> inkwell::values::BasicValueEnum<'c> {
    let compile_expression = |expression: &_| compile_constant(context, expression, variables);

    match expression {
        Expression::AlignOf(align_of) => compile_align_of(context, align_of).into(),
        Expression::ArithmeticOperation(operation) => compile_arithmetic_operation(
            &context.inkwell().create_builder(),
            operation,
            &compile_expression,
        ),
        Expression::BitCast(bit_cast) => compile_constant_bit_cast(
            context,
            &context.inkwell().create_builder(),
            bit_cast,
            &compile_expression,
        ),
        Expression::BitwiseNotOperation(operation) => compile_bitwise_not_operation(
            &context.inkwell().create_builder(),
            operation,
            &compile_expression,
        )
        .into(),
        Expression::BitwiseOperation(operation) => compile_bitwise_operation(
            &context.inkwell().create_builder(),
            operation,
            &compile_expression,
        )
        .into(),
        Expression::ComparisonOperation(operation) => compile_comparison_operation(
            &context.inkwell().create_builder(),
            operation,
            &compile_expression,
        ),
        Expression::PointerAddress(address) => compile_pointer_address(
            &context.inkwell().create_builder(),
            address,
            &compile_expression,
        )
        .into(),
        Expression::Primitive(primitive) => compile_primitive(context, *primitive),
        Expression::Record(record) => context
            .inkwell()
            .const_struct(
                &record
                    .fields()
                    .iter()
                    .map(|expression| compile_expression(expression))
                    .collect::<Vec<_>>(),
                false,
            )
            .into(),
        Expression::RecordAddress(address) => compile_record_address(
            context,
            &context.inkwell().create_builder(),
            address,
            &compile_expression,
        )
        .into(),
        Expression::SizeOf(size_of) => compile_size_of(context, size_of).into(),
        Expression::Undefined(undefined) => compile_undefined(context, undefined),
        Expression::Union(union) => context
            .inkwell()
            .const_struct(
                &[
                    compile_expression(union.member()),
                    type_::compile_union_member_padding(
                        context,
                        union.type_(),
                        union.member_index(),
                    )
                    .const_zero()
                    .into(),
                ],
                false,
            )
            .into(),
        Expression::UnionAddress(address) => compile_union_address(
            context,
            &context.inkwell().create_builder(),
            address,
            &compile_expression,
        )
        .into(),
        Expression::Variable(variable) => variables[variable.name()],
    }
}

fn compile_align_of<'c>(
    context: &Context<'c>,
    align_of: &AlignOf,
) -> inkwell::values::IntValue<'c> {
    compile_pointer_integer(
        context,
        context
            .target_data()
            .get_abi_alignment(&type_::compile(context, align_of.type_())) as u64,
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
    context: &Context<'c>,
    builder: &inkwell::builder::Builder<'c>,
    bit_cast: &BitCast,
    compile_expression: &impl Fn(&Expression) -> inkwell::values::BasicValueEnum<'c>,
) -> inkwell::values::BasicValueEnum<'c> {
    if is_constant_bit_cast_supported(bit_cast.from())
        && is_constant_bit_cast_supported(bit_cast.to())
    {
        compile_constant_bit_cast(context, builder, bit_cast, compile_expression)
    } else {
        let pointer = builder.build_alloca(type_::compile(context, bit_cast.from()), "");

        builder.build_store(pointer, compile_expression(bit_cast.expression()));

        builder.build_load(
            builder
                .build_bitcast(
                    pointer,
                    type_::compile_pointer(
                        context,
                        &fmm::types::Pointer::new(bit_cast.to().clone()),
                    ),
                    "",
                )
                .into_pointer_value(),
            "",
        )
    }
}

fn compile_constant_bit_cast<'c>(
    context: &Context<'c>,
    builder: &inkwell::builder::Builder<'c>,
    bit_cast: &BitCast,
    compile_expression: &impl Fn(&Expression) -> inkwell::values::BasicValueEnum<'c>,
) -> inkwell::values::BasicValueEnum<'c> {
    let argument = compile_expression(bit_cast.expression());
    let to_type = type_::compile(context, bit_cast.to());

    let value = builder.build_bitcast(
        if argument.is_pointer_value() {
            builder
                .build_ptr_to_int(
                    argument.into_pointer_value(),
                    type_::compile_pointer_integer(context),
                    "",
                )
                .into()
        } else {
            argument
        },
        if to_type.is_pointer_type() {
            type_::compile_pointer_integer(context).into()
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
        fmm::ir::BitwiseOperator::LeftShift => builder.build_left_shift(lhs, rhs, ""),
        fmm::ir::BitwiseOperator::RightShift(signed) => {
            builder.build_right_shift(lhs, rhs, signed, "")
        }
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
    context: &Context<'c>,
    builder: &inkwell::builder::Builder<'c>,
    address: &RecordAddress,
    compile_expression: &impl Fn(&Expression) -> inkwell::values::BasicValueEnum<'c>,
) -> inkwell::values::PointerValue<'c> {
    unsafe {
        builder.build_gep(
            compile_expression(address.pointer()).into_pointer_value(),
            &[
                context.inkwell().i32_type().const_zero(),
                context
                    .inkwell()
                    .i32_type()
                    .const_int(address.field_index() as u64, false),
            ],
            "",
        )
    }
}

fn compile_size_of<'c>(context: &Context<'c>, size_of: &SizeOf) -> inkwell::values::IntValue<'c> {
    compile_pointer_integer(
        context,
        context
            .target_data()
            .get_store_size(&type_::compile(context, size_of.type_())),
    )
}

fn compile_undefined<'c>(
    context: &Context<'c>,
    undefined: &Undefined,
) -> inkwell::values::BasicValueEnum<'c> {
    match undefined.type_() {
        fmm::types::Type::Function(function) => type_::compile_function_pointer(context, function)
            .const_zero()
            .into(),
        fmm::types::Type::Primitive(primitive) => compile_undefined_primitive(context, *primitive),
        fmm::types::Type::Pointer(pointer) => {
            type_::compile_pointer(context, pointer).const_zero().into()
        }
        fmm::types::Type::Record(record) => {
            type_::compile_record(context, record).const_zero().into()
        }
        fmm::types::Type::Union(union) => type_::compile_union(context, union).const_zero().into(),
    }
}

// TODO Is signed options for the const_int method correct?
fn compile_primitive<'c>(
    context: &Context<'c>,
    primitive: Primitive,
) -> inkwell::values::BasicValueEnum<'c> {
    match primitive {
        Primitive::Boolean(boolean) => context
            .inkwell()
            .bool_type()
            .const_int(boolean as u64, false)
            .into(),
        Primitive::Float32(number) => context
            .inkwell()
            .f32_type()
            .const_float(number as f64)
            .into(),
        Primitive::Float64(number) => context
            .inkwell()
            .f64_type()
            .const_float(number)
            .into(),
        Primitive::Integer8(number) => context
            .inkwell()
            .i8_type()
            .const_int(number as u64, false)
            .into(),
        Primitive::Integer32(number) => context
            .inkwell()
            .i32_type()
            .const_int(number as u64, false)
            .into(),
        Primitive::Integer64(number) => {
            context.inkwell().i64_type().const_int(number, false).into()
        }
        Primitive::PointerInteger(number) => compile_pointer_integer(context, number as u64).into(),
    }
}

pub fn compile_pointer_integer<'c>(
    context: &Context<'c>,
    number: u64,
) -> inkwell::values::IntValue<'c> {
    type_::compile_pointer_integer(context).const_int(number, false)
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
    context: &Context<'c>,
    type_: fmm::types::Primitive,
) -> inkwell::values::BasicValueEnum<'c> {
    match type_::compile_primitive(context, type_) {
        inkwell::types::BasicTypeEnum::FloatType(float) => float.const_zero().into(),
        inkwell::types::BasicTypeEnum::IntType(integer) => integer.const_zero().into(),
        inkwell::types::BasicTypeEnum::PointerType(pointer) => pointer.const_zero().into(),
        _ => unreachable!(),
    }
}

fn compile_union_address<'c>(
    context: &Context<'c>,
    builder: &inkwell::builder::Builder<'c>,
    address: &UnionAddress,
    compile_expression: &impl Fn(&Expression) -> inkwell::values::BasicValueEnum<'c>,
) -> inkwell::values::PointerValue<'c> {
    unsafe {
        builder.build_gep(
            builder
                .build_bitcast(
                    compile_expression(address.pointer()),
                    type_::compile_union_member(context, address.type_(), address.member_index())
                        .ptr_type(Default::default()),
                    "",
                )
                .into_pointer_value(),
            &[
                context.inkwell().i32_type().const_zero(),
                context.inkwell().i32_type().const_zero(),
            ],
            "",
        )
    }
}

fn compile_integer_comparison_operator(operator: ComparisonOperator) -> inkwell::IntPredicate {
    match operator {
        ComparisonOperator::Equal => inkwell::IntPredicate::EQ,
        ComparisonOperator::NotEqual => inkwell::IntPredicate::NE,
        ComparisonOperator::LessThan(false) => inkwell::IntPredicate::ULT,
        ComparisonOperator::LessThan(true) => inkwell::IntPredicate::SLT,
        ComparisonOperator::LessThanOrEqual(false) => inkwell::IntPredicate::ULE,
        ComparisonOperator::LessThanOrEqual(true) => inkwell::IntPredicate::SLE,
        ComparisonOperator::GreaterThan(false) => inkwell::IntPredicate::UGT,
        ComparisonOperator::GreaterThan(true) => inkwell::IntPredicate::SGT,
        ComparisonOperator::GreaterThanOrEqual(false) => inkwell::IntPredicate::UGE,
        ComparisonOperator::GreaterThanOrEqual(true) => inkwell::IntPredicate::SGE,
    }
}

fn compile_float_comparison_operator(operator: ComparisonOperator) -> inkwell::FloatPredicate {
    match operator {
        ComparisonOperator::Equal => inkwell::FloatPredicate::OEQ,
        ComparisonOperator::NotEqual => inkwell::FloatPredicate::ONE,
        ComparisonOperator::LessThan(_) => inkwell::FloatPredicate::OLT,
        ComparisonOperator::LessThanOrEqual(_) => inkwell::FloatPredicate::OLE,
        ComparisonOperator::GreaterThan(_) => inkwell::FloatPredicate::OGT,
        ComparisonOperator::GreaterThanOrEqual(_) => inkwell::FloatPredicate::OGE,
    }
}
