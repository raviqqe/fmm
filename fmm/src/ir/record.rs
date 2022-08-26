use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct Record {
    type_: types::Record,
    fields: Arc<[Expression]>,
}

impl Record {
    pub fn new(type_: types::Record, fields: Vec<Expression>) -> Self {
        Self {
            type_,
            fields: fields.into(),
        }
    }

    pub fn type_(&self) -> &types::Record {
        &self.type_
    }

    pub fn fields(&self) -> &[Expression] {
        &self.fields
    }
}
