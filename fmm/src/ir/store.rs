use super::expression::Expression;
use crate::types::Type;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct Store {
    type_: Type, // pointer element type
    value: Rc<Expression>,
    pointer: Rc<Expression>,
}

impl Store {
    pub fn new(
        type_: impl Into<Type>,
        value: impl Into<Expression>,
        pointer: impl Into<Expression>,
    ) -> Self {
        Self {
            type_: type_.into(),
            value: value.into().into(),
            pointer: pointer.into().into(),
        }
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }

    pub fn pointer(&self) -> &Expression {
        &self.pointer
    }
}
