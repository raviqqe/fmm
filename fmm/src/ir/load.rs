use super::expression::Expression;
use crate::types::Type;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct Load {
    type_: Type, // pointer element type
    pointer: Rc<Expression>,
    name: String,
}

impl Load {
    pub fn new(
        type_: impl Into<Type>,
        pointer: impl Into<Expression>,
        name: impl Into<String>,
    ) -> Self {
        Self {
            type_: type_.into(),
            pointer: pointer.into().into(),
            name: name.into(),
        }
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }

    pub fn pointer(&self) -> &Expression {
        &self.pointer
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
