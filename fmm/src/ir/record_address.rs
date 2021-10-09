use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct RecordAddress {
    type_: types::Record,
    pointer: Arc<Expression>, // pointer to record
    field_index: usize,
}

impl RecordAddress {
    pub fn new(type_: types::Record, pointer: impl Into<Expression>, field_index: usize) -> Self {
        Self {
            type_,
            pointer: pointer.into().into(),
            field_index,
        }
    }

    pub fn type_(&self) -> &types::Record {
        &self.type_
    }

    pub fn pointer(&self) -> &Expression {
        &self.pointer
    }

    pub fn field_index(&self) -> usize {
        self.field_index
    }
}
