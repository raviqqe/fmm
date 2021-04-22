use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct BitwiseNotOperation {
    type_: types::Primitive,
    value: Arc<Expression>,
}

impl BitwiseNotOperation {
    pub fn new(type_: types::Primitive, value: impl Into<Expression>) -> Self {
        Self {
            type_,
            value: Arc::new(value.into()),
        }
    }

    pub fn type_(&self) -> types::Primitive {
        self.type_
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }
}
