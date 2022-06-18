use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct DeconstructUnion {
    type_: types::Union,
    union: Arc<Expression>,
    member_index: usize,
    name: String,
}

impl DeconstructUnion {
    pub fn new(
        type_: types::Union,
        union: impl Into<Expression>,
        member_index: usize,
        name: impl Into<String>,
    ) -> Self {
        Self {
            type_,
            union: union.into().into(),
            member_index,
            name: name.into(),
        }
    }

    pub fn type_(&self) -> &types::Union {
        &self.type_
    }

    pub fn union(&self) -> &Expression {
        &self.union
    }

    pub fn member_index(&self) -> usize {
        self.member_index
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
