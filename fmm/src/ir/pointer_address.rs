use super::expression::Expression;
use crate::types;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct PointerAddress {
    type_: types::Pointer, // type of the pointer value
    pointer: Rc<Expression>,
    offset: Rc<Expression>,
}

impl PointerAddress {
    pub fn new(
        type_: types::Pointer,
        pointer: impl Into<Expression>,
        offset: impl Into<Expression>,
    ) -> Self {
        Self {
            type_,
            pointer: pointer.into().into(),
            offset: offset.into().into(),
        }
    }

    pub fn type_(&self) -> &types::Pointer {
        &self.type_
    }

    pub fn pointer(&self) -> &Expression {
        &self.pointer
    }

    pub fn offset(&self) -> &Expression {
        &self.offset
    }
}
