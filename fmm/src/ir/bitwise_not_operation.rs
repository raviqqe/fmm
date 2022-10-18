use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct BitwiseNotOperation(Arc<BitwiseNotOperationInner>);

#[derive(Clone, Debug, PartialEq)]
struct BitwiseNotOperationInner {
    type_: types::Primitive,
    value: Expression,
}

impl BitwiseNotOperation {
    pub fn new(type_: types::Primitive, value: impl Into<Expression>) -> Self {
        Self(
            BitwiseNotOperationInner {
                type_,
                value: value.into(),
            }
            .into(),
        )
    }

    pub fn type_(&self) -> types::Primitive {
        self.0.type_
    }

    pub fn value(&self) -> &Expression {
        &self.0.value
    }
}
