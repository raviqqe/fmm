use super::{atomic_ordering::AtomicOrdering, expression::Expression};
use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct AtomicStore(Box<AtomicStoreInner>);

#[derive(Clone, Debug, PartialEq)]
struct AtomicStoreInner {
    type_: Type, // pointer element type
    value: Expression,
    pointer: Expression,
    ordering: AtomicOrdering,
}

impl AtomicStore {
    pub fn new(
        type_: impl Into<Type>,
        value: impl Into<Expression>,
        pointer: impl Into<Expression>,
        ordering: AtomicOrdering,
    ) -> Self {
        Self(
            AtomicStoreInner {
                type_: type_.into(),
                value: value.into(),
                pointer: pointer.into(),
                ordering,
            }
            .into(),
        )
    }

    pub fn type_(&self) -> &Type {
        &self.0.type_
    }

    pub fn type_mut(&mut self) -> &mut Type {
        &mut self.0.type_
    }

    pub fn value(&self) -> &Expression {
        &self.0.value
    }

    pub fn value_mut(&mut self) -> &mut Expression {
        &mut self.0.value
    }

    pub fn pointer(&self) -> &Expression {
        &self.0.pointer
    }

    pub fn pointer_mut(&mut self) -> &mut Expression {
        &mut self.0.pointer
    }

    pub fn ordering(&self) -> AtomicOrdering {
        self.0.ordering
    }
}
