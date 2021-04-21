use super::{typed_expression::TypedExpression, BuildError};
use crate::ir::*;
use crate::types::{self, Type};

pub fn align_of(type_: impl Into<Type>) -> TypedExpression {
    AlignOf::new(type_.into()).into()
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

pub fn record(elements: Vec<TypedExpression>) -> Record {
    Record::new(
        types::Record::new(
            elements
                .iter()
                .map(|element| element.type_().clone())
                .collect(),
        ),
        elements
            .iter()
            .map(|element| element.expression().clone())
            .collect(),
    )
}

pub fn size_of(type_: impl Into<Type>) -> TypedExpression {
    SizeOf::new(type_.into()).into()
}

pub fn variable(name: impl Into<String>, type_: impl Into<Type>) -> TypedExpression {
    TypedExpression::new(Variable::new(name), type_)
}
