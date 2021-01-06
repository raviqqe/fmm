use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct AtomicLoad {
    type_: types::Primitive, // pointer element type
    pointer: Arc<Expression>,
    name: String,
}

impl AtomicLoad {
    pub fn new(
        type_: types::Primitive,
        pointer: impl Into<Expression>,
        name: impl Into<String>,
    ) -> Self {
        Self {
            type_,
            pointer: pointer.into().into(),
            name: name.into(),
        }
    }

    pub fn type_(&self) -> types::Primitive {
        self.type_
    }

    pub fn pointer(&self) -> &Expression {
        &self.pointer
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
