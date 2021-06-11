use super::{atomic_ordering::AtomicOrdering, expression::Expression};
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
    ordering: AtomicOrdering,
    name: String,
}

impl AtomicOperation {
    pub fn new(
        type_: types::Primitive,
        operator: AtomicOperator,
        pointer: impl Into<Expression>,
        value: impl Into<Expression>,
        ordering: AtomicOrdering,
        name: impl Into<String>,
    ) -> Self {
        Self {
            type_,
            operator,
            pointer: pointer.into().into(),
            value: value.into().into(),
            ordering,
            name: name.into(),
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

    pub fn ordering(&self) -> AtomicOrdering {
        self.ordering
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
