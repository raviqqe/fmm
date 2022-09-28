use crate::{
    build::{self, BuildError, InstructionBuilder, TypedExpression},
    ir::*,
    types::{self, generic_pointer_type, Type},
};
use once_cell::unsync::Lazy;

const DEFAULT_STACK_SIZE: i64 = 64;

thread_local! {
    static STACK_TYPE: Lazy<Type> = Lazy::new(|| {
        types::Pointer::new(types::Record::new(vec![
            generic_pointer_type(), // base pointer
            types::Primitive::PointerInteger.into(), // size
            types::Primitive::PointerInteger.into(), // capacity
        ]))
        .into()
    });
}

pub fn stack_type() -> Type {
    STACK_TYPE.with(|type_| (**type_).clone())
}

pub fn create_stack(builder: &InstructionBuilder) -> Result<TypedExpression, BuildError> {
    let capacity = Primitive::PointerInteger(DEFAULT_STACK_SIZE);
    let pointer = builder.allocate_heap(capacity);
    let record = build::record(vec![
        pointer,
        Primitive::PointerInteger(0).into(),
        capacity.into(),
    ]);

    let pointer = builder.allocate_stack(record.type_().clone());
    builder.store(record, pointer.clone());

    Ok(pointer)
}

pub fn destroy_stack(
    builder: &InstructionBuilder,
    stack_pointer: impl Into<TypedExpression>,
) -> Result<(), BuildError> {
    builder.free_heap(builder.load(build::record_address(stack_pointer, 0)?)?);

    Ok(())
}

pub fn push_to_stack(
    builder: &InstructionBuilder,
    stack: impl Into<TypedExpression>,
    element: impl Into<TypedExpression>,
) -> Result<(), BuildError> {
    let stack = stack.into();
    let element = element.into();

    let size = builder.load(build::record_address(stack.clone(), 1)?)?;
    let new_size = build::arithmetic_operation(
        ArithmeticOperator::Add,
        size,
        get_element_size(builder, element.type_())?,
    )?;
    let capacity = builder.load(build::record_address(stack.clone(), 2)?)?;

    builder.if_(
        build::comparison_operation(
            ComparisonOperator::GreaterThan(false),
            new_size.clone(),
            capacity.clone(),
        )?,
        |builder| {
            let pointer = build::record_address(stack.clone(), 0)?;

            let new_capacity = build::arithmetic_operation(
                ArithmeticOperator::Multiply,
                capacity.clone(),
                Primitive::PointerInteger(2),
            )?;
            builder.store(
                builder.reallocate_heap(builder.load(pointer.clone())?, new_capacity.clone()),
                pointer,
            );
            builder.store(new_capacity, build::record_address(stack.clone(), 2)?);

            Ok(builder.branch(void_value()))
        },
        |builder| Ok(builder.branch(void_value())),
    )?;

    builder.store(
        element.clone(),
        get_element_pointer(builder, &stack, element.type_())?,
    );
    builder.store(new_size, build::record_address(stack, 1)?);

    Ok(())
}

pub fn pop_from_stack(
    builder: &InstructionBuilder,
    stack: impl Into<TypedExpression>,
    type_: impl Into<Type>,
) -> Result<TypedExpression, BuildError> {
    let type_ = type_.into();
    let stack = stack.into();

    builder.store(
        build::arithmetic_operation(
            ArithmeticOperator::Subtract,
            builder.load(build::record_address(stack.clone(), 1)?)?,
            get_element_size(builder, &type_)?,
        )?,
        build::record_address(stack.clone(), 1)?,
    );

    builder.load(get_element_pointer(builder, &stack, &type_)?)
}

fn get_element_pointer(
    builder: &InstructionBuilder,
    stack: &TypedExpression,
    type_: &Type,
) -> Result<TypedExpression, BuildError> {
    Ok(build::bit_cast(
        types::Pointer::new(type_.clone()),
        build::pointer_address(
            builder.load(build::record_address(stack.clone(), 0)?)?,
            builder.load(build::record_address(stack.clone(), 1)?)?,
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
