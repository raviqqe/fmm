use std::{
    cell::{RefCell},
    rc::Rc,
};

use crate::{
    build::{self, BuildError, InstructionBuilder, NameGenerator, TypedExpression},
    ir::*,
    types::{self, generic_pointer_type, void_type, Type},
};
use once_cell::unsync::Lazy;

const EXTEND_FUNCTION_NAME: &str = "_fmm_stack_extend";
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

pub fn type_() -> Type {
    STACK_TYPE.with(|type_| (**type_).clone())
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
    let pointer = element_pointer(builder, &stack, element.type_())?;

    builder.call(
        build::variable(
            EXTEND_FUNCTION_NAME,
            types::Function::new(
                vec![type_(), types::Primitive::PointerInteger.into()],
                void_type(),
                types::CallingConvention::Target,
            ),
        ),
        vec![stack, element_size(builder, element.type_())?],
    )?;

    builder.store(element, pointer);

    Ok(())
}

pub fn define_utility_functions(module: &Module) -> Result<Module, BuildError> {
    Ok(Module::new(
        module.variable_declarations().to_vec(),
        module.function_declarations().to_vec(),
        module.variable_definitions().to_vec(),
        module
            .function_definitions()
            .iter()
            .cloned()
            .chain([extend_function_definition()?])
            .collect(),
    ))
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
        build::variable(ELEMENT_SIZE_NAME, types::Primitive::PointerInteger),
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

    builder.store(new_size, build::record_address(stack, 1)?);

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

pub fn pop(
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
            element_size(builder, &type_)?,
        )?,
        build::record_address(stack.clone(), 1)?,
    );

    builder.load(element_pointer(builder, &stack, &type_)?)
}

fn element_pointer(
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

fn element_size(builder: &InstructionBuilder, type_: &Type) -> Result<TypedExpression, BuildError> {
    align_size(builder, build::size_of(type_.clone()))
}

// TODO Support 16-byte aligned data.
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
