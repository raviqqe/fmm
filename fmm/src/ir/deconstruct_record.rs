use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct DeconstructRecord {
    type_: types::Record,
    record: Arc<Expression>,
    element_index: usize,
    name: String,
}

impl DeconstructRecord {
    pub fn new(
        type_: types::Record,
        record: impl Into<Expression>,
        element_index: usize,
        name: impl Into<String>,
    ) -> Self {
        Self {
            type_,
            record: record.into().into(),
            element_index,
            name: name.into(),
        }
    }

    pub fn type_(&self) -> &types::Record {
        &self.type_
    }

    pub fn record(&self) -> &Expression {
        &self.record
    }

    pub fn element_index(&self) -> usize {
        self.element_index
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
