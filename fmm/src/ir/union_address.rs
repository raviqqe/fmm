use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct UnionAddress {
    type_: types::Union,
    pointer: Arc<Expression>, // pointer to union
    member_index: usize,
    name: String,
}

impl UnionAddress {
    pub fn new(
        type_: types::Union,
        pointer: impl Into<Expression>,
        member_index: usize,
        name: impl Into<String>,
    ) -> Self {
        Self {
            type_,
            pointer: pointer.into().into(),
            member_index,
            name: name.into(),
        }
    }

    pub fn type_(&self) -> &types::Union {
        &self.type_
    }

    pub fn pointer(&self) -> &Expression {
        &self.pointer
    }

    pub fn member_index(&self) -> usize {
        self.member_index
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
