use super::{atomic_ordering::AtomicOrdering, expression::Expression};
use crate::types;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AtomicOperator {
    Add,
    Subtract,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AtomicOperation(Box<AtomicOperationInner>);

#[derive(Clone, Debug, PartialEq)]
struct AtomicOperationInner {
    type_: types::Primitive,
    operator: AtomicOperator,
    pointer: Expression,
    value: Expression,
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
        Self(
            AtomicOperationInner {
                type_,
                operator,
                pointer: pointer.into(),
                value: value.into(),
                ordering,
                name: name.into(),
            }
            .into(),
        )
    }

    pub fn type_(&self) -> types::Primitive {
        self.0.type_
    }

    pub fn type_mut(&mut self) -> &mut types::Primitive {
        &mut self.0.type_
    }

    pub fn operator(&self) -> AtomicOperator {
        self.0.operator
    }

    pub fn pointer(&self) -> &Expression {
        &self.0.pointer
    }

    pub fn pointer_mut(&mut self) -> &mut Expression {
        &mut self.0.pointer
    }

    pub fn value(&self) -> &Expression {
        &self.0.value
    }

    pub fn value_mut(&mut self) -> &mut Expression {
        &mut self.0.value
    }

    pub fn ordering(&self) -> AtomicOrdering {
        self.0.ordering
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }
}
