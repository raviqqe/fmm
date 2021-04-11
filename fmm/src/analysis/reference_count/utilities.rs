use super::error::ReferenceCountError;
use crate::{
    build::{self, BuildError, InstructionBuilder, TypedExpression, VOID_VALUE},
    ir::*,
    types,
};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

pub fn if_heap_pointer(
    builder: &InstructionBuilder,
    pointer: &TypedExpression,
    then: impl Fn(&InstructionBuilder) -> Result<(), ReferenceCountError>,
) -> Result<(), ReferenceCountError> {
    builder.if_(
        builder.comparison_operation(
            ComparisonOperator::NotEqual,
            build::bit_cast(types::Primitive::PointerInteger, pointer.clone()),
            Undefined::new(types::Primitive::PointerInteger),
        )?,
        |builder| -> Result<_, ReferenceCountError> {
            builder.if_(
                builder.comparison_operation(
                    ComparisonOperator::NotEqual,
                    build::bitwise_operation(
                        BitwiseOperator::And,
                        build::bit_cast(types::Primitive::PointerInteger, pointer.clone()),
                        Primitive::PointerInteger(1),
                    )?,
                    Primitive::PointerInteger(1),
                )?,
                |builder| -> Result<_, ReferenceCountError> {
                    then(&builder)?;
                    Ok(builder.branch(VOID_VALUE.clone()))
                },
                |builder| Ok(builder.branch(VOID_VALUE.clone())),
            )?;
            Ok(builder.branch(VOID_VALUE.clone()))
        },
        |builder| Ok(builder.branch(VOID_VALUE.clone())),
    )?;

    Ok(())
}

pub fn get_counter_pointer(
    builder: &InstructionBuilder,
    heap_pointer: &TypedExpression,
) -> Result<TypedExpression, BuildError> {
    builder.pointer_address(
        build::bit_cast(
            types::Pointer::new(types::Primitive::PointerInteger),
            heap_pointer.clone(),
        ),
        Primitive::PointerInteger(-1),
    )
}

pub fn extract_record_elements(
    builder: &InstructionBuilder,
    variable: &Variable,
    record_type: &types::Record,
) -> Result<Vec<TypedExpression>, BuildError> {
    record_type
        .elements()
        .iter()
        .enumerate()
        .map(|(index, _)| {
            builder.deconstruct_record(build::variable(variable.name(), record_type.clone()), index)
        })
        .collect()
}

pub fn get_record_clone_function_name(record: &types::Record) -> String {
    format!("rc_record_clone_{:x}", hash_record_type(record))
}

pub fn get_record_drop_function_name(record: &types::Record) -> String {
    format!("rc_record_drop_{:x}", hash_record_type(record))
}

fn hash_record_type(record: &types::Record) -> u64 {
    let mut hasher = DefaultHasher::new();

    record.hash(&mut hasher);

    hasher.finish()
}
