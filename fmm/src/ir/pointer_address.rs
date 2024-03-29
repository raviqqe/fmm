use super::expression::Expression;
use crate::types;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct PointerAddress(Rc<PointerAddressInner>);

#[derive(Clone, Debug, PartialEq)]
struct PointerAddressInner {
    // TODO Change this to an element type?
    type_: types::Pointer, // type of the pointer value
    pointer: Expression,
    offset: Expression,
}

impl PointerAddress {
    pub fn new(
        type_: types::Pointer,
        pointer: impl Into<Expression>,
        offset: impl Into<Expression>,
    ) -> Self {
        Self(
            PointerAddressInner {
                type_,
                pointer: pointer.into(),
                offset: offset.into(),
            }
            .into(),
        )
    }

    pub fn type_(&self) -> &types::Pointer {
        &self.0.type_
    }

    pub fn pointer(&self) -> &Expression {
        &self.0.pointer
    }

    pub fn offset(&self) -> &Expression {
        &self.0.offset
    }
}
