use super::{atomic_ordering::AtomicOrdering, expression::Expression};
use crate::types::Type;


#[derive(Clone, Debug, PartialEq)]
pub struct CompareAndSwap(Box<CompareAndSwapInner>);

#[derive(Clone, Debug, PartialEq)]
struct CompareAndSwapInner {
    type_: Type, // pointer element type
    pointer: Expression,
    old_value: Expression,
    new_value: Expression,
    success_ordering: AtomicOrdering,
    failure_ordering: AtomicOrdering,
    name: String,
}

impl CompareAndSwap {
    pub fn new(
        type_: impl Into<Type>,
        pointer: impl Into<Expression>,
        old_value: impl Into<Expression>,
        new_value: impl Into<Expression>,
        success_ordering: AtomicOrdering,
        failure_ordering: AtomicOrdering,
        name: impl Into<String>,
    ) -> Self {
        Self(
            CompareAndSwapInner {
                type_: type_.into(),
                pointer: pointer.into(),
                old_value: old_value.into(),
                new_value: new_value.into(),
                success_ordering,
                failure_ordering,
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

    pub fn old_value(&self) -> &Expression {
        &self.0.old_value
    }

    pub fn new_value(&self) -> &Expression {
        &self.0.new_value
    }

    pub fn success_ordering(&self) -> AtomicOrdering {
        self.0.success_ordering
    }

    pub fn failure_ordering(&self) -> AtomicOrdering {
        self.0.failure_ordering
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }
}
