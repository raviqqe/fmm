use super::expression::Expression;
use crate::types;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct Union(Rc<UnionInner>);

#[derive(Clone, Debug, PartialEq)]
struct UnionInner {
    type_: types::Union,
    member_index: usize,
    member: Expression,
}

impl Union {
    pub fn new(type_: types::Union, member_index: usize, member: impl Into<Expression>) -> Self {
        Self(
            UnionInner {
                type_,
                member_index,
                member: member.into(),
            }
            .into(),
        )
    }

    pub fn type_(&self) -> &types::Union {
        &self.0.type_
    }

    pub fn member_index(&self) -> usize {
        self.0.member_index
    }

    pub fn member(&self) -> &Expression {
        &self.0.member
    }
}
