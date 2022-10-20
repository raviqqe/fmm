use super::expression::Expression;
use crate::types::Type;


#[derive(Clone, Debug, PartialEq)]
pub struct Store(Box<StoreInner>);

#[derive(Clone, Debug, PartialEq)]
struct StoreInner {
    type_: Type, // pointer element type
    value: Expression,
    pointer: Expression,
}

impl Store {
    pub fn new(
        type_: impl Into<Type>,
        value: impl Into<Expression>,
        pointer: impl Into<Expression>,
    ) -> Self {
        Self(
            StoreInner {
                type_: type_.into(),
                value: value.into(),
                pointer: pointer.into(),
            }
            .into(),
        )
    }

    pub fn type_(&self) -> &Type {
        &self.0.type_
    }

    pub fn value(&self) -> &Expression {
        &self.0.value
    }

    pub fn pointer(&self) -> &Expression {
        &self.0.pointer
    }
}
