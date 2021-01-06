use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct AtomicStore {
    type_: types::Primitive, // pointer element type
    value: Arc<Expression>,
    pointer: Arc<Expression>,
}

impl AtomicStore {
    pub fn new(
        type_: types::Primitive,
        value: impl Into<Expression>,
        pointer: impl Into<Expression>,
    ) -> Self {
        Self {
            type_,
            value: value.into().into(),
            pointer: pointer.into().into(),
        }
    }

    pub fn type_(&self) -> types::Primitive {
        self.type_
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }

    pub fn pointer(&self) -> &Expression {
        &self.pointer
    }
}
