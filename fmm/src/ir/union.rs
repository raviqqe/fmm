use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct Union {
    type_: types::Union,
    member_index: usize,
    member: Arc<Expression>,
}

impl Union {
    pub fn new(type_: types::Union, member_index: usize, member: impl Into<Expression>) -> Self {
        Self {
            type_,
            member_index,
            member: member.into().into(),
        }
    }

    pub fn type_(&self) -> &types::Union {
        &self.type_
    }

    pub fn member_index(&self) -> usize {
        self.member_index
    }

    pub fn member(&self) -> &Expression {
        &self.member
    }
}
