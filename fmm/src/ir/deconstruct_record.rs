use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct DeconstructRecord(Arc<DeconstructRecordInner>);

#[derive(Clone, Debug, PartialEq)]
struct DeconstructRecordInner {
    type_: types::Record,
    record: Expression,
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
        Self(
            DeconstructRecordInner {
                type_,
                record: record.into().into(),
                field_index,
                name: name.into(),
            }
            .into(),
        )
    }

    pub fn type_(&self) -> &types::Record {
        &self.0.type_
    }

    pub fn record(&self) -> &Expression {
        &self.0.record
    }

    pub fn field_index(&self) -> usize {
        self.0.field_index
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }
}
