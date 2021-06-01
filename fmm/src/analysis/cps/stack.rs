use crate::{
    build::{self, BuildError, InstructionBuilder, TypedExpression, VOID_VALUE},
    ir::*,
    types::{self, Type, GENERIC_POINTER_TYPE},
};
use once_cell::sync::Lazy;

pub static STACK_TYPE: Lazy<Type> = Lazy::new(|| {
    types::Pointer::new(types::Record::new(vec![
        GENERIC_POINTER_TYPE.clone(),            // base pointer
        types::Primitive::PointerInteger.into(), // size
        types::Primitive::PointerInteger.into(), // capacity
    ]))
    .into()
});

pub fn push_to_stack(
    builder: &InstructionBuilder,
    stack: impl Into<TypedExpression>,
    element: impl Into<TypedExpression>,
) -> Result<(), BuildError> {
    let stack = stack.into();
    let element = element.into();

    let size = builder.load(builder.record_address(stack.clone(), 1)?)?;
    let new_size = build::arithmetic_operation(
        ArithmeticOperator::Add,
        size,
        get_element_size(builder, element.type_())?,
    )?;
    let capacity = builder.load(builder.record_address(stack.clone(), 2)?)?;

    builder.if_(
        build::comparison_operation(
            ComparisonOperator::GreaterThan,
            new_size.clone(),
            capacity.clone(),
        )?,
        |builder| {
            let pointer = builder.record_address(stack.clone(), 0)?;

            let new_capacity = build::arithmetic_operation(
                ArithmeticOperator::Multiply,
                capacity.clone(),
                Primitive::PointerInteger(2),
            )?;
            builder.store(
                builder.reallocate_heap(builder.load(pointer.clone())?, new_capacity.clone()),
                pointer,
            );
            builder.store(new_capacity, builder.record_address(stack.clone(), 2)?);

            Ok(builder.branch(VOID_VALUE.clone()))
        },
        |builder| Ok(builder.branch(VOID_VALUE.clone())),
    )?;

    builder.store(
        element.clone(),
        get_element_pointer(builder, &stack, element.type_())?,
    );
    builder.store(new_size, builder.record_address(stack, 1)?);

    Ok(())
}

pub fn pop_from_stack(
    builder: &InstructionBuilder,
    stack: impl Into<TypedExpression>,
    type_: &Type,
) -> Result<TypedExpression, BuildError> {
    let stack = stack.into();

    builder.store(
        build::arithmetic_operation(
            ArithmeticOperator::Subtract,
            builder.load(builder.record_address(stack.clone(), 1)?)?,
            get_element_size(builder, type_)?,
        )?,
        builder.record_address(stack.clone(), 1)?,
    );

    builder.load(get_element_pointer(builder, &stack, type_)?)
}

fn get_element_pointer(
    builder: &InstructionBuilder,
    stack: &TypedExpression,
    type_: &Type,
) -> Result<TypedExpression, BuildError> {
    Ok(build::bit_cast(
        types::Pointer::new(type_.clone()),
        builder.pointer_address(
            builder.load(builder.record_address(stack.clone(), 0)?)?,
            builder.load(builder.record_address(stack.clone(), 1)?)?,
        )?,
    )
    .into())
}

fn get_element_size(
    builder: &InstructionBuilder,
    type_: &Type,
) -> Result<TypedExpression, BuildError> {
    align_size(builder, build::size_of(type_.clone()))
}

// TODO Support 16-byte data.
fn align_size(
    builder: &InstructionBuilder,
    size: impl Into<TypedExpression>,
) -> Result<TypedExpression, BuildError> {
    let size = size.into();
    let alignment = build::align_of(types::Primitive::PointerInteger);

    builder.if_(
        build::comparison_operation(
            ComparisonOperator::Equal,
            size.clone(),
            Primitive::PointerInteger(0),
        )?,
        |builder| Ok(builder.branch(Primitive::PointerInteger(0))),
        |builder| {
            Ok(builder.branch(build::arithmetic_operation(
                ArithmeticOperator::Multiply,
                build::arithmetic_operation(
                    ArithmeticOperator::Add,
                    build::arithmetic_operation(
                        ArithmeticOperator::Divide,
                        build::arithmetic_operation(
                            ArithmeticOperator::Subtract,
                            size.clone(),
                            Primitive::PointerInteger(1),
                        )?,
                        alignment.clone(),
                    )?,
                    Primitive::PointerInteger(1),
                )?,
                alignment.clone(),
            )?))
        },
    )
}
