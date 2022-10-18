use super::expression::Expression;
use crate::types::Type;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct Store(Arc<StoreInner>);

#[derive(Clone, Debug, PartialEq)]
struct StoreInner {
    type_: Type, // pointer element type
    value: Arc<Expression>,
    pointer: Arc<Expression>,
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
                value: value.into().into(),
                pointer: pointer.into().into(),
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
