use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct DeconstructRecord {
    type_: types::Record,
    record: Arc<Expression>,
    field_index: usize,
    name: String,
}

impl DeconstructRecord {
    pub fn new(
        type_: types::Record,
        record: impl Into<Expression>,
        field_index: usize,
        name: impl Into<String>,
    ) -> Self {
        Self {
            type_,
            record: record.into().into(),
            field_index,
            name: name.into(),
        }
    }

    pub fn type_(&self) -> &types::Record {
        &self.type_
    }

    pub fn record(&self) -> &Expression {
        &self.record
    }

    pub fn field_index(&self) -> usize {
        self.field_index
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
