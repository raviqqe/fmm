use super::expression::Expression;
use crate::types;

#[derive(Clone, Debug, PartialEq)]
pub struct DeconstructRecord(Box<DeconstructRecordInner>);

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
                record: record.into(),
                field_index,
                name: name.into(),
            }
            .into(),
        )
    }

    pub fn type_(&self) -> &types::Record {
        &self.0.type_
    }

    pub fn type_mut(&mut self) -> &mut types::Record {
        &mut self.0.type_
    }

    pub fn record(&self) -> &Expression {
        &self.0.record
    }

    pub fn record_mut(&mut self) -> &mut Expression {
        &mut self.0.record
    }

    pub fn field_index(&self) -> usize {
        self.0.field_index
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }
}
