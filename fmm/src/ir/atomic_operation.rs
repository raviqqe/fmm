use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AtomicOperator {
    Add,
    Subtract,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AtomicOperation {
    type_: types::Primitive,
    operator: AtomicOperator,
    pointer: Arc<Expression>,
    value: Arc<Expression>,
}

impl AtomicOperation {
    pub fn new(
        type_: types::Primitive,
        operator: AtomicOperator,
        pointer: impl Into<Expression>,
        value: impl Into<Expression>,
    ) -> Self {
        Self {
            type_,
            operator,
            pointer: pointer.into().into(),
            value: value.into().into(),
        }
    }

    pub fn type_(&self) -> types::Primitive {
        self.type_
    }

    pub fn operator(&self) -> AtomicOperator {
        self.operator
    }

    pub fn pointer(&self) -> &Expression {
        &self.pointer
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }
}
