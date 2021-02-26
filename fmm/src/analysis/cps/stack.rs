use crate::build::{self, InstructionBuilder, TypedExpression};
use crate::ir::*;
use crate::types::{self, Type};
use once_cell::sync::Lazy;

pub static STACK_TYPE: Lazy<Type> = Lazy::new(|| {
    types::Pointer::new(types::Record::new(vec![
        types::Pointer::new(types::Primitive::Integer8).into(), // base pointer
        types::Primitive::PointerInteger.into(),                // size
        types::Primitive::PointerInteger.into(),                // capacity
    ]))
    .into()
});

pub fn push_to_stack(
    builder: &InstructionBuilder,
    stack: impl Into<TypedExpression>,
    element: impl Into<TypedExpression>,
) {
    let stack = stack.into();
    let element = element.into();

    let size = builder.load(builder.record_address(stack.clone(), 1));
    // TODO Align stack sizes.
    let new_size = builder.arithmetic_operation(
        ArithmeticOperator::Add,
        size.clone(),
        build::size_of(element.type_().clone()),
    );
    let capacity = builder.load(builder.record_address(stack.clone(), 2));

    builder.if_(
        builder.comparison_operation(
            ComparisonOperator::GreaterThan,
            new_size.clone(),
            capacity.clone(),
        ),
        |builder| {
            let pointer = builder.record_address(stack.clone(), 0);

            let new_capacity = builder.arithmetic_operation(
                ArithmeticOperator::Multiply,
                capacity.clone(),
                Primitive::PointerInteger(2),
            );
            builder.store(
                builder.reallocate_heap(builder.load(pointer.clone()), new_capacity.clone()),
                pointer.clone(),
            );
            builder.store(new_capacity, builder.record_address(stack.clone(), 2));

            builder.branch(Undefined::new(types::Primitive::Boolean))
        },
        |builder| builder.branch(Undefined::new(types::Primitive::Boolean)),
    );

    builder.store(
        element.clone(),
        build::bit_cast(
            types::Pointer::new(element.type_().clone()),
            builder.pointer_address(builder.load(builder.record_address(stack.clone(), 0)), size),
        ),
    );
    builder.store(new_size, builder.record_address(stack.clone(), 1));
}

pub fn pop_from_stack(
    builder: &InstructionBuilder,
    stack: impl Into<TypedExpression>,
    type_: &Type,
) -> TypedExpression {
    let stack = stack.into();

    let new_size = builder.arithmetic_operation(
        ArithmeticOperator::Subtract,
        builder.load(builder.record_address(stack.clone(), 1)),
        build::size_of(type_.clone()),
    );

    let element = builder.load(build::bit_cast(
        types::Pointer::new(type_.clone()),
        builder.load(builder.record_address(stack.clone(), 0)),
    ));
    builder.store(new_size, builder.record_address(stack.clone(), 1));

    element
}
