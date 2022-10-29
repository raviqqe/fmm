use crate::{
    build::{self, BuildError, InstructionBuilder, NameGenerator, TypedExpression},
    ir::*,
    types::{self, generic_pointer_type, void_type, Type},
};
use once_cell::unsync::Lazy;
use std::{cell::RefCell, rc::Rc};

const EXTEND_FUNCTION_NAME: &str = "_fmm_stack_extend";
const ALIGN_SIZE_FUNCTION_NAME: &str = "_fmm_stack_align_size";
const DEFAULT_STACK_SIZE: i64 = 64;
const MAX_ALIGNMENT_TYPE: types::Primitive = types::Primitive::PointerInteger;

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

pub fn type_() -> Type {
    STACK_TYPE.with(|type_| (*type_).clone())
}

pub fn create(builder: &InstructionBuilder) -> Result<TypedExpression, BuildError> {
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

pub fn destroy(
    builder: &InstructionBuilder,
    stack_pointer: impl Into<TypedExpression>,
) -> Result<(), BuildError> {
    builder.free_heap(builder.load(build::record_address(stack_pointer, 0)?)?);

    Ok(())
}

pub fn push(
    builder: &InstructionBuilder,
    stack: impl Into<TypedExpression>,
    element: impl Into<TypedExpression>,
) -> Result<(), BuildError> {
    let stack = stack.into();
    let element = element.into();

    extend(builder, &stack, element.type_())?;
    push_one(builder, &stack, &element)?;
    align_size(builder, &stack, &MAX_ALIGNMENT_TYPE.into())?;

    Ok(())
}

pub fn pop(
    builder: &InstructionBuilder,
    stack: impl Into<TypedExpression>,
    type_: impl Into<Type>,
) -> Result<TypedExpression, BuildError> {
    let type_ = type_.into();
    let stack = stack.into();
    let size = build::arithmetic_operation(
        ArithmeticOperator::Subtract,
        builder.load(build::record_address(stack.clone(), 1)?)?,
        align(builder, build::size_of(type_.clone()))?,
    )?;

    builder.store(size.clone(), build::record_address(stack.clone(), 1)?);

    builder.load(element_pointer(builder, &stack, &size.into(), &type_)?)
}

pub fn partial_push(
    builder: &InstructionBuilder,
    stack: impl Into<TypedExpression>,
    old_environment: &[(&str, &Type)],
    new_environment: &[(&str, &Type)],
) -> Result<(), BuildError> {
    let stack = stack.into();

    extend(
        builder,
        &stack,
        &create_environment_record(new_environment)
            .type_()
            .clone()
            .into(),
    )?;

    let index = old_environment
        .iter()
        .zip(new_environment)
        .position(|(one, other)| one != other)
        .unwrap_or_else(|| old_environment.len().max(new_environment.len()));

    increase_size(
        builder,
        &stack,
        &create_environment_record(&new_environment[..index])
            .type_()
            .clone()
            .into(),
    )?;

    for (name, type_) in &new_environment[index..] {
        push_one(builder, &stack, &build::variable(*name, (*type_).clone()))?;
    }

    align_size(builder, &stack, &MAX_ALIGNMENT_TYPE.into())?;

    Ok(())
}

fn push_one(
    builder: &InstructionBuilder,
    stack: &TypedExpression,
    element: &TypedExpression,
) -> Result<(), BuildError> {
    align_size(builder, &stack, element.type_())?;
    builder.store(
        element.clone(),
        element_pointer(
            builder,
            &stack,
            &builder.load(build::record_address(stack.clone(), 1)?)?,
            element.type_(),
        )?,
    );
    increase_size(builder, &stack, element.type_())?;

    Ok(())
}

pub fn define_utility_functions(module: &mut Module) -> Result<(), BuildError> {
    module.function_definitions_mut().extend([
        extend_function_definition()?,
        align_size_function_definition()?,
    ]);

    Ok(())
}

fn extend(
    builder: &InstructionBuilder,
    stack: &TypedExpression,
    element_type: &Type,
) -> Result<(), BuildError> {
    builder.call(
        build::variable(
            EXTEND_FUNCTION_NAME,
            types::Function::new(
                vec![type_(), types::Primitive::PointerInteger.into()],
                void_type(),
                types::CallingConvention::Target,
            ),
        ),
        vec![stack.clone(), build::size_of(element_type.clone())],
    )?;

    Ok(())
}

fn element_pointer(
    builder: &InstructionBuilder,
    stack: &TypedExpression,
    size: &TypedExpression,
    type_: &Type,
) -> Result<TypedExpression, BuildError> {
    Ok(build::bit_cast(
        types::Pointer::new(type_.clone()),
        build::pointer_address(
            builder.load(build::record_address(stack.clone(), 0)?)?,
            size.clone(),
        )?,
    )
    .into())
}

fn increase_size(
    builder: &InstructionBuilder,
    stack: &TypedExpression,
    type_: &Type,
) -> Result<(), BuildError> {
    let size_pointer = build::record_address(stack.clone(), 1)?;

    builder.store(
        build::arithmetic_operation(
            ArithmeticOperator::Add,
            builder.load(size_pointer.clone())?,
            build::size_of(type_.clone()),
        )?,
        size_pointer,
    );

    Ok(())
}

fn align_size(
    builder: &InstructionBuilder,
    stack: &TypedExpression,
    type_: &Type,
) -> Result<(), BuildError> {
    let size_pointer = build::record_address(stack.clone(), 1)?;

    builder.store(
        align_v2(builder, &builder.load(size_pointer.clone())?, type_)?,
        size_pointer,
    );

    Ok(())
}

fn align_v2(
    builder: &InstructionBuilder,
    size: &TypedExpression,
    type_: &Type,
) -> Result<TypedExpression, BuildError> {
    let alignment = build::align_of(type_.clone());

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

fn align(
    builder: &InstructionBuilder,
    size: impl Into<TypedExpression>,
) -> Result<TypedExpression, BuildError> {
    builder.call(
        build::variable(
            ALIGN_SIZE_FUNCTION_NAME,
            types::Function::new(
                vec![types::Primitive::PointerInteger.into()],
                types::Primitive::PointerInteger,
                types::CallingConvention::Target,
            ),
        ),
        vec![size.into()],
    )
}

fn extend_function_definition() -> Result<FunctionDefinition, BuildError> {
    const STACK_NAME: &str = "s";
    const ELEMENT_SIZE_NAME: &str = "e";

    let builder = InstructionBuilder::new(Rc::new(RefCell::new(NameGenerator::new("x"))));

    let stack = build::variable(STACK_NAME, type_());
    let size = builder.load(build::record_address(stack.clone(), 1)?)?;
    let new_size = build::arithmetic_operation(
        ArithmeticOperator::Add,
        size,
        align(
            &builder,
            build::variable(ELEMENT_SIZE_NAME, types::Primitive::PointerInteger),
        )?,
    )?;
    let capacity = builder.load(build::record_address(stack.clone(), 2)?)?;

    // TODO Handle elements larger than stack increase sizes.
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

    Ok(FunctionDefinition::new(
        EXTEND_FUNCTION_NAME,
        vec![
            Argument::new(STACK_NAME, type_()),
            Argument::new(ELEMENT_SIZE_NAME, types::Primitive::PointerInteger),
        ],
        void_type(),
        builder.return_(void_value()),
        FunctionDefinitionOptions::new()
            .set_calling_convention(types::CallingConvention::Target)
            .set_linkage(Linkage::Internal),
    ))
}

// TODO Support 16-byte aligned data.
fn align_size_function_definition() -> Result<FunctionDefinition, BuildError> {
    const SIZE_NAME: &str = "size";

    let builder = InstructionBuilder::new(Rc::new(RefCell::new(NameGenerator::new("x"))));
    let size = build::variable(SIZE_NAME, types::Primitive::PointerInteger);
    let alignment = build::align_of(types::Primitive::PointerInteger);

    let value = builder.if_(
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
    )?;

    Ok(FunctionDefinition::new(
        ALIGN_SIZE_FUNCTION_NAME,
        vec![Argument::new(SIZE_NAME, types::Primitive::PointerInteger)],
        types::Primitive::PointerInteger,
        builder.return_(value),
        FunctionDefinitionOptions::new()
            .set_calling_convention(types::CallingConvention::Target)
            .set_linkage(Linkage::Internal),
    ))
}

// TODO Share this with `source_function.rs`.
fn create_environment_record(environment: &[(&str, &Type)]) -> Record {
    build::record(
        environment
            .iter()
            .map(|(name, type_)| build::variable(*name, (*type_).clone()))
            .collect(),
    )
}
