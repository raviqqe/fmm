use super::expression::Expression;
use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct Load(Box<LoadInner>);

#[derive(Clone, Debug, PartialEq)]
struct LoadInner {
    type_: Type, // pointer element type
    pointer: Expression,
    name: Rc<str>,
}

impl Load {
    pub fn new(
        type_: impl Into<Type>,
        pointer: impl Into<Expression>,
        name: impl Into<Rc<str>>,
    ) -> Self {
        Self(
            LoadInner {
                type_: type_.into(),
                pointer: pointer.into(),
                name: name.into(),
            }
            .into(),
        )
    }

    pub fn type_(&self) -> &Type {
        &self.0.type_
    }

    pub fn type_mut(&mut self) -> &mut Type {
        &mut self.0.type_
    }

    pub fn pointer(&self) -> &Expression {
        &self.0.pointer
    }

    pub fn pointer_mut(&mut self) -> &mut Expression {
        &mut self.0.pointer
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }
}
