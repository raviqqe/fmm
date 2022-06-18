use super::{atomic_ordering::AtomicOrdering, expression::Expression};
use crate::types::Type;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct AtomicLoad {
    type_: Type, // pointer element type
    pointer: Rc<Expression>,
    ordering: AtomicOrdering,
    name: String,
}

impl AtomicLoad {
    pub fn new(
        type_: impl Into<Type>,
        pointer: impl Into<Expression>,
        ordering: AtomicOrdering,
        name: impl Into<String>,
    ) -> Self {
        Self {
            type_: type_.into(),
            pointer: pointer.into().into(),
            ordering,
            name: name.into(),
        }
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }

    pub fn pointer(&self) -> &Expression {
        &self.pointer
    }

    pub fn ordering(&self) -> AtomicOrdering {
        self.ordering
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
