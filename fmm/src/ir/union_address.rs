use super::expression::Expression;
use crate::types;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct UnionAddress {
    type_: types::Union,
    pointer: Rc<Expression>, // pointer to union
    member_index: usize,
}

impl UnionAddress {
    pub fn new(type_: types::Union, pointer: impl Into<Expression>, member_index: usize) -> Self {
        Self {
            type_,
            pointer: pointer.into().into(),
            member_index,
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
}
