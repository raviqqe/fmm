use super::{atomic_ordering::AtomicOrdering, expression::Expression};
use crate::types::Type;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct AtomicStore {
    type_: Type, // pointer element type
    value: Rc<Expression>,
    pointer: Rc<Expression>,
    ordering: AtomicOrdering,
}

impl AtomicStore {
    pub fn new(
        type_: impl Into<Type>,
        value: impl Into<Expression>,
        pointer: impl Into<Expression>,
        ordering: AtomicOrdering,
    ) -> Self {
        Self {
            type_: type_.into(),
            value: value.into().into(),
            pointer: pointer.into().into(),
            ordering,
        }
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }

    pub fn pointer(&self) -> &Expression {
        &self.pointer
    }

    pub fn ordering(&self) -> AtomicOrdering {
        self.ordering
    }
}
