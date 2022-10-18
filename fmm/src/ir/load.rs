use super::expression::Expression;
use crate::types::Type;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct Load(Arc<LoadInner>);

#[derive(Clone, Debug, PartialEq)]
struct LoadInner {
    type_: Type, // pointer element type
    pointer: Expression,
    name: String,
}

impl Load {
    pub fn new(
        type_: impl Into<Type>,
        pointer: impl Into<Expression>,
        name: impl Into<String>,
    ) -> Self {
        Self(
            LoadInner {
                type_: type_.into(),
                pointer: pointer.into().into(),
                name: name.into(),
            }
            .into(),
        )
    }

    pub fn type_(&self) -> &Type {
        &self.0.type_
    }

    pub fn pointer(&self) -> &Expression {
        &self.0.pointer
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }
}
