use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct TaggedUnion {
    type_: types::TaggedUnion,
    member_index: usize,
    payload: Arc<Expression>,
}

impl TaggedUnion {
    pub fn new(
        type_: types::TaggedUnion,
        member_index: usize,
        payload: impl Into<Expression>,
    ) -> Self {
        Self {
            type_,
            member_index,
            payload: payload.into().into(),
        }
    }

    pub fn type_(&self) -> &types::TaggedUnion {
        &self.type_
    }

    pub fn member_index(&self) -> usize {
        self.member_index
    }

    pub fn payload(&self) -> &Expression {
        &self.payload
    }
}
