use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct PointerAddress {
    type_: types::Pointer, // type of the pointer value
    pointer: Arc<Expression>,
    offset: Arc<Expression>,
    name: String,
}

impl PointerAddress {
    pub fn new(
        type_: types::Pointer,
        pointer: impl Into<Expression>,
        offset: impl Into<Expression>,
        name: impl Into<String>,
    ) -> Self {
        Self {
            type_,
            pointer: pointer.into().into(),
            offset: offset.into().into(),
            name: name.into(),
        }
    }

    pub fn type_(&self) -> &types::Pointer {
        &self.type_
    }

    pub fn pointer(&self) -> &Expression {
        &self.pointer
    }

    pub fn offset(&self) -> &Expression {
        &self.offset
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
