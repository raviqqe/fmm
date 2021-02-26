use super::typed_expression::TypedExpression;
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
