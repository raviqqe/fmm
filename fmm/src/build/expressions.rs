use super::{typed_expression::TypedExpression, BuildError};
use crate::{
    ir::*,
    types::{self, Type},
};

pub fn align_of(type_: impl Into<Type>) -> TypedExpression {
    AlignOf::new(type_.into()).into()
}

pub fn arithmetic_operation(
    operator: ArithmeticOperator,
    lhs: impl Into<TypedExpression>,
    rhs: impl Into<TypedExpression>,
) -> Result<ArithmeticOperation, BuildError> {
    let lhs = lhs.into();
    let rhs = rhs.into();

    Ok(ArithmeticOperation::new(
        lhs.type_()
            .to_primitive()
            .ok_or_else(|| BuildError::PrimitiveExpected(lhs.type_().clone()))?,
        operator,
        lhs.expression().clone(),
        rhs.expression().clone(),
    ))
}

pub fn bit_cast(to: impl Into<Type>, expression: impl Into<TypedExpression>) -> BitCast {
    let expression = expression.into();

    BitCast::new(
        expression.type_().clone(),
        to,
        expression.expression().clone(),
    )
}

pub fn bitwise_not_operation(
    value: impl Into<TypedExpression>,
) -> Result<BitwiseNotOperation, BuildError> {
    let value = value.into();

    Ok(BitwiseNotOperation::new(
        value
            .type_()
            .to_primitive()
            .ok_or_else(|| BuildError::PrimitiveExpected(value.type_().clone()))?,
        value.expression().clone(),
    ))
}

pub fn bitwise_operation(
    operator: BitwiseOperator,
    lhs: impl Into<TypedExpression>,
    rhs: impl Into<TypedExpression>,
) -> Result<BitwiseOperation, BuildError> {
    let lhs = lhs.into();
    let rhs = rhs.into();

    Ok(BitwiseOperation::new(
        lhs.type_()
            .to_primitive()
            .ok_or_else(|| BuildError::PrimitiveExpected(lhs.type_().clone()))?,
        operator,
        lhs.expression().clone(),
        rhs.expression().clone(),
    ))
}

pub fn comparison_operation(
    operator: ComparisonOperator,
    lhs: impl Into<TypedExpression>,
    rhs: impl Into<TypedExpression>,
) -> Result<ComparisonOperation, BuildError> {
    let lhs = lhs.into();
    let rhs = rhs.into();

    Ok(ComparisonOperation::new(
        lhs.type_()
            .to_primitive()
            .ok_or_else(|| BuildError::PrimitiveExpected(lhs.type_().clone()))?,
        operator,
        lhs.expression().clone(),
        rhs.expression().clone(),
    ))
}

pub fn pointer_address(
    pointer: impl Into<TypedExpression>,
    offset: impl Into<TypedExpression>,
) -> Result<PointerAddress, BuildError> {
    let pointer = pointer.into();
    let offset = offset.into();
    let type_ = pointer
        .type_()
        .to_pointer()
        .ok_or_else(|| BuildError::PointerExpected(pointer.type_().clone()))?
        .clone();

    Ok(PointerAddress::new(
        type_,
        pointer.expression().clone(),
        offset.expression().clone(),
    ))
}

pub fn record(fields: Vec<TypedExpression>) -> Record {
    Record::new(
        types::Record::new(
            fields
                .iter()
                .map(|field| field.type_().clone())
                .collect(),
        ),
        fields
            .iter()
            .map(|field| field.expression().clone())
            .collect(),
    )
}

pub fn record_address(
    pointer: impl Into<TypedExpression>,
    field_index: usize,
) -> Result<RecordAddress, BuildError> {
    let pointer = pointer.into();
    let element_type = pointer
        .type_()
        .to_pointer()
        .ok_or_else(|| BuildError::PointerExpected(pointer.type_().clone()))?
        .element();
    let type_ = element_type
        .to_record()
        .ok_or_else(|| BuildError::RecordExpected(element_type.clone()))?
        .clone();

    Ok(RecordAddress::new(
        type_,
        pointer.expression().clone(),
        field_index,
    ))
}

pub fn size_of(type_: impl Into<Type>) -> TypedExpression {
    SizeOf::new(type_.into()).into()
}

pub fn union_address(
    pointer: impl Into<TypedExpression>,
    member_index: usize,
) -> Result<UnionAddress, BuildError> {
    let pointer = pointer.into();
    let element_type = pointer
        .type_()
        .to_pointer()
        .ok_or_else(|| BuildError::PointerExpected(pointer.type_().clone()))?
        .element();
    let type_ = element_type
        .to_union()
        .ok_or_else(|| BuildError::UnionExpected(element_type.clone()))?
        .clone();

    Ok(UnionAddress::new(
        type_,
        pointer.expression().clone(),
        member_index,
    ))
}

pub fn variable(name: impl Into<String>, type_: impl Into<Type>) -> TypedExpression {
    TypedExpression::new(Variable::new(name), type_)
}
