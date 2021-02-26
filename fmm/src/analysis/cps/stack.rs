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
    stack: TypedExpression,
    element: TypedExpression,
) {
    let new_size = builder.arithmetic_operation(
        ArithmeticOperator::Add,
        builder.load(builder.record_address(stack.clone(), 1)),
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

            builder.store(
                pointer.clone(),
                builder.reallocate_heap(
                    builder.load(pointer),
                    builder.arithmetic_operation(
                        ArithmeticOperator::Multiply,
                        capacity.clone(),
                        Primitive::PointerInteger(2),
                    ),
                ),
            );

            builder.branch(Undefined::new(types::Primitive::Boolean))
        },
        |builder| builder.branch(Undefined::new(types::Primitive::Boolean)),
    );

    builder.store(
        element.clone(),
        build::bit_cast(
            types::Pointer::new(element.type_().clone()),
            builder.load(builder.record_address(stack.clone(), 0)),
        ),
    );
}
