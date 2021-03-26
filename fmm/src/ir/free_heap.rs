use super::expression::Expression;
use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct FreeHeap {
    type_: Type,
    pointer: Expression,
}

impl FreeHeap {
    pub fn new(type_: impl Into<Type>, pointer: impl Into<Expression>) -> Self {
        Self {
            type_: type_.into(),
            pointer: pointer.into(),
        }
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }

    pub fn pointer(&self) -> &Expression {
        &self.pointer
    }
}
