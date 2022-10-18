use super::expression::Expression;
use crate::types;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct UnionAddress(Rc<UnionAddressInner>);

#[derive(Clone, Debug, PartialEq)]
struct UnionAddressInner {
    type_: types::Union,
    pointer: Expression, // pointer to union
    member_index: usize,
}

impl UnionAddress {
    pub fn new(type_: types::Union, pointer: impl Into<Expression>, member_index: usize) -> Self {
        Self(
            UnionAddressInner {
                type_,
                pointer: pointer.into(),
                member_index,
            }
            .into(),
        )
    }

    pub fn type_(&self) -> &types::Union {
        &self.0.type_
    }

    pub fn pointer(&self) -> &Expression {
        &self.0.pointer
    }

    pub fn member_index(&self) -> usize {
        self.0.member_index
    }
}
