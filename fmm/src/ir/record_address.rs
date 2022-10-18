use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct RecordAddress(Arc<RecordAddressInner>);

#[derive(Clone, Debug, PartialEq)]
struct RecordAddressInner {
    type_: types::Record,
    pointer: Expression, // pointer to record
    field_index: usize,
}

impl RecordAddress {
    pub fn new(type_: types::Record, pointer: impl Into<Expression>, field_index: usize) -> Self {
        Self(
            RecordAddressInner {
                type_,
                pointer: pointer.into(),
                field_index,
            }
            .into(),
        )
    }

    pub fn type_(&self) -> &types::Record {
        &self.0.type_
    }

    pub fn pointer(&self) -> &Expression {
        &self.0.pointer
    }

    pub fn field_index(&self) -> usize {
        self.0.field_index
    }
}
