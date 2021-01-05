use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct RecordAddress {
    type_: types::Record,
    pointer: Arc<Expression>, // pointer to record
    element_index: usize,
    name: String,
}

impl RecordAddress {
    pub fn new(
        type_: types::Record,
        pointer: impl Into<Expression>,
        element_index: usize,
        name: impl Into<String>,
    ) -> Self {
        Self {
            type_,
            pointer: pointer.into().into(),
            element_index,
            name: name.into(),
        }
    }

    pub fn type_(&self) -> &types::Record {
        &self.type_
    }

    pub fn pointer(&self) -> &Expression {
        &self.pointer
    }

    pub fn element_index(&self) -> usize {
        self.element_index
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
