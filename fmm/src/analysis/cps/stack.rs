use crate::build::{self, InstructionBuilder, TypedExpression};
use crate::ir::*;
use crate::types::{self, Type};
use once_cell::sync::Lazy;

static VOID_TYPE: Lazy<Type> = Lazy::new(|| types::Record::new(vec![]).into());

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
    let new_size = builder.arithmetic_operation(
        ArithmeticOperator::Add,
        size.clone(),
        get_element_size(builder, element.type_()),
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
                pointer,
            );
            builder.store(new_capacity, builder.record_address(stack.clone(), 2));

            builder.branch(Undefined::new(VOID_TYPE.clone()))
        },
        |builder| builder.branch(Undefined::new(VOID_TYPE.clone())),
    );

    builder.store(
        element.clone(),
        builder.load(get_element_pointer(builder, &stack, element.type_())),
    );
    builder.store(new_size, builder.record_address(stack, 1));
}

pub fn pop_from_stack(
    builder: &InstructionBuilder,
    stack: impl Into<TypedExpression>,
    type_: &Type,
) -> TypedExpression {
    let stack = stack.into();

    builder.store(
        builder.arithmetic_operation(
            ArithmeticOperator::Subtract,
            builder.load(builder.record_address(stack.clone(), 1)),
            get_element_size(builder, type_),
        ),
        builder.record_address(stack.clone(), 1),
    );

    builder.load(get_element_pointer(builder, &stack, type_))
}

fn get_element_pointer(
    builder: &InstructionBuilder,
    stack: &TypedExpression,
    type_: &Type,
) -> TypedExpression {
    build::bit_cast(
        types::Pointer::new(type_.clone()),
        builder.pointer_address(
            builder.load(builder.record_address(stack.clone(), 0)),
            builder.load(builder.record_address(stack.clone(), 1)),
        ),
    )
    .into()
}

fn get_element_size(builder: &InstructionBuilder, type_: &Type) -> TypedExpression {
    align_size(builder, build::size_of(type_.clone()))
}

// TODO Support 16-byte data.
fn align_size(builder: &InstructionBuilder, size: impl Into<TypedExpression>) -> TypedExpression {
    let size = size.into();
    let alignment = build::align_of(types::Primitive::PointerInteger);

    builder.if_(
        builder.comparison_operation(
            ComparisonOperator::Equal,
            size.clone(),
            Primitive::PointerInteger(0),
        ),
        |builder| builder.branch(Primitive::PointerInteger(0)),
        |builder| {
            builder.branch(builder.arithmetic_operation(
                ArithmeticOperator::Multiply,
                builder.arithmetic_operation(
                    ArithmeticOperator::Add,
                    builder.arithmetic_operation(
                        ArithmeticOperator::Divide,
                        builder.arithmetic_operation(
                            ArithmeticOperator::Subtract,
                            size.clone(),
                            Primitive::PointerInteger(1),
                        ),
                        alignment.clone(),
                    ),
                    Primitive::PointerInteger(1),
                ),
                alignment.clone(),
            ))
        },
    )
}
