use super::atomic_ordering::AtomicOrdering;
use super::expression::Expression;
use crate::types::Type;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct CompareAndSwap {
    type_: Type, // pointer element type
    pointer: Arc<Expression>,
    old_value: Arc<Expression>,
    new_value: Arc<Expression>,
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
        Self {
            type_: type_.into(),
            pointer: pointer.into().into(),
            old_value: old_value.into().into(),
            new_value: new_value.into().into(),
            success_ordering,
            failure_ordering,
            name: name.into(),
        }
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }

    pub fn pointer(&self) -> &Expression {
        &self.pointer
    }

    pub fn old_value(&self) -> &Expression {
        &self.old_value
    }

    pub fn new_value(&self) -> &Expression {
        &self.new_value
    }

    pub fn success_ordering(&self) -> AtomicOrdering {
        self.success_ordering
    }

    pub fn failure_ordering(&self) -> AtomicOrdering {
        self.failure_ordering
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
