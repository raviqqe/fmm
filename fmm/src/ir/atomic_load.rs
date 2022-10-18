use super::{atomic_ordering::AtomicOrdering, expression::Expression};
use crate::types::Type;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct AtomicLoad(Arc<AtomicLoadInner>);

#[derive(Clone, Debug, PartialEq)]
struct AtomicLoadInner {
    type_: Type, // pointer element type
    pointer: Expression,
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
        Self(
            AtomicLoadInner {
                type_: type_.into(),
                pointer: pointer.into().into(),
                ordering,
                name: name.into(),
            }
            .into(),
        )
    }

    pub fn type_(&self) -> &Type {
        &self.0.type_
    }

    pub fn pointer(&self) -> &Expression {
        &self.0.pointer
    }

    pub fn ordering(&self) -> AtomicOrdering {
        self.0.ordering
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }
}
