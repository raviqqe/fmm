use super::contextual_expression::*;
use super::names::*;
use crate::ir::*;
use crate::types;

pub fn arithmetic_operation(
    operator: ArithmeticOperator,
    lhs: impl Into<ContextualExpression>,
    rhs: impl Into<ContextualExpression>,
) -> ContextualExpression {
    let lhs = lhs.into();
    let rhs = rhs.into();
    let type_ = lhs.type_().to_primitive().unwrap();
    let name = generate_name();

    ContextualExpression::new(
        lhs.instructions()
            .iter()
            .chain(rhs.instructions())
            .cloned()
            .chain(vec![ArithmeticOperation::new(
                type_,
                operator,
                lhs.expression().clone(),
                rhs.expression().clone(),
                &name,
            )
            .into()]),
        Variable::new(name),
        type_,
    )
}

pub fn atomic_load(pointer: impl Into<ContextualExpression>) -> ContextualExpression {
    let pointer = pointer.into();
    let type_ = pointer.type_().to_pointer().unwrap().element().clone();
    let name = generate_name();

    ContextualExpression::new(
        pointer
            .instructions()
            .iter()
            .cloned()
            .chain(vec![AtomicLoad::new(
                type_.clone(),
                pointer.expression().clone(),
                &name,
            )
            .into()]),
        Variable::new(name),
        type_,
    )
}

pub fn call(
    function: impl Into<ContextualExpression>,
    arguments: impl IntoIterator<Item = ContextualExpression>,
) -> ContextualExpression {
    let function = function.into();
    let arguments = arguments.into_iter().collect::<Vec<_>>();
    let type_ = function.type_().to_function().unwrap().clone();
    let name = generate_name();

    ContextualExpression::new(
        function
            .instructions()
            .iter()
            .chain(arguments.iter().flat_map(|context| context.instructions()))
            .cloned()
            .chain(vec![Call::new(
                type_.clone(),
                function.expression().clone(),
                arguments
                    .iter()
                    .map(|context| context.expression())
                    .cloned()
                    .collect(),
                &name,
            )
            .into()]),
        Variable::new(name),
        type_.result().clone(),
    )
}

pub fn comparison_operation(
    operator: ComparisonOperator,
    lhs: impl Into<ContextualExpression>,
    rhs: impl Into<ContextualExpression>,
) -> ContextualExpression {
    let lhs = lhs.into();
    let rhs = rhs.into();
    let name = generate_name();

    ContextualExpression::new(
        lhs.instructions()
            .iter()
            .chain(rhs.instructions())
            .cloned()
            .chain(vec![ComparisonOperation::new(
                lhs.type_().to_primitive().unwrap(),
                operator,
                lhs.expression().clone(),
                rhs.expression().clone(),
                &name,
            )
            .into()]),
        Variable::new(name),
        types::Primitive::Bool,
    )
}

pub fn deconstruct_record(
    record: impl Into<ContextualExpression>,
    element_index: usize,
) -> ContextualExpression {
    let record = record.into();
    let type_ = record.type_().to_record().unwrap().clone();
    let name = generate_name();

    ContextualExpression::new(
        record
            .instructions()
            .iter()
            .cloned()
            .chain(vec![DeconstructRecord::new(
                type_.clone(),
                record.expression().clone(),
                element_index,
                &name,
            )
            .into()]),
        Variable::new(name),
        type_.elements()[element_index].clone(),
    )
}

pub fn deconstruct_union(
    union: impl Into<ContextualExpression>,
    member_index: usize,
) -> ContextualExpression {
    let union = union.into();
    let type_ = union.type_().to_union().unwrap().clone();
    let name = generate_name();

    ContextualExpression::new(
        union
            .instructions()
            .iter()
            .cloned()
            .chain(vec![DeconstructUnion::new(
                type_.clone(),
                union.expression().clone(),
                member_index,
                &name,
            )
            .into()]),
        Variable::new(name),
        type_.members()[member_index].clone(),
    )
}

pub fn load(pointer: impl Into<ContextualExpression>) -> ContextualExpression {
    let pointer = pointer.into();
    let type_ = pointer.type_().to_pointer().unwrap().element().clone();
    let name = generate_name();

    ContextualExpression::new(
        pointer.instructions().iter().cloned().chain(vec![Load::new(
            type_.clone(),
            pointer.expression().clone(),
            &name,
        )
        .into()]),
        Variable::new(name),
        type_,
    )
}

pub fn pointer_address(
    pointer: impl Into<ContextualExpression>,
    offset: impl Into<ContextualExpression>,
) -> ContextualExpression {
    let pointer = pointer.into();
    let offset = offset.into();
    let type_ = pointer.type_().to_pointer().unwrap().clone();
    let name = generate_name();

    ContextualExpression::new(
        pointer
            .instructions()
            .iter()
            .chain(offset.instructions())
            .cloned()
            .chain(vec![PointerAddress::new(
                type_.clone(),
                pointer.expression().clone(),
                offset.expression().clone(),
                &name,
            )
            .into()]),
        Variable::new(name),
        type_,
    )
}

pub fn record_address(
    pointer: impl Into<ContextualExpression>,
    element_index: usize,
) -> ContextualExpression {
    let pointer = pointer.into();
    let type_ = pointer
        .type_()
        .to_pointer()
        .unwrap()
        .element()
        .to_record()
        .unwrap()
        .clone();
    let name = generate_name();

    ContextualExpression::new(
        pointer
            .instructions()
            .iter()
            .cloned()
            .chain(vec![RecordAddress::new(
                type_.clone(),
                pointer.expression().clone(),
                element_index,
                &name,
            )
            .into()]),
        Variable::new(name),
        types::Pointer::new(type_.elements()[element_index].clone()),
    )
}

pub fn union_address(
    pointer: impl Into<ContextualExpression>,
    member_index: usize,
) -> ContextualExpression {
    let pointer = pointer.into();
    let type_ = pointer
        .type_()
        .to_pointer()
        .unwrap()
        .element()
        .to_union()
        .unwrap()
        .clone();
    let name = generate_name();

    ContextualExpression::new(
        pointer
            .instructions()
            .iter()
            .cloned()
            .chain(vec![UnionAddress::new(
                type_.clone(),
                pointer.expression().clone(),
                member_index,
                &name,
            )
            .into()]),
        Variable::new(name),
        types::Pointer::new(type_.members()[member_index].clone()),
    )
}
