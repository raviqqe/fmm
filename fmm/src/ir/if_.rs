use super::{block::Block, expression::Expression};
use crate::types::Type;
use indexmap::IndexSet;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct If(Box<IfInner>);

#[derive(Clone, Debug, PartialEq)]
struct IfInner {
    type_: Type,
    condition: Expression,
    then: Block,
    else_: Block,
    name: String,
    environment: IndexSet<Rc<str>>,
}

impl If {
    pub fn new(
        type_: impl Into<Type>,
        condition: impl Into<Expression>,
        then: Block,
        else_: Block,
        name: impl Into<String>,
    ) -> Self {
        Self(
            IfInner {
                type_: type_.into(),
                condition: condition.into(),
                then,
                else_,
                name: name.into(),
                environment: Default::default(),
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

    pub fn condition(&self) -> &Expression {
        &self.0.condition
    }

    pub fn condition_mut(&mut self) -> &mut Expression {
        &mut self.0.condition
    }

    pub fn then(&self) -> &Block {
        &self.0.then
    }

    pub fn then_mut(&mut self) -> &mut Block {
        &mut self.0.then
    }

    pub fn else_(&self) -> &Block {
        &self.0.else_
    }

    pub fn else_mut(&mut self) -> &mut Block {
        &mut self.0.else_
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }

    pub fn name_mut(&mut self) -> &mut String {
        &mut self.0.name
    }

    pub fn environment(&self) -> &IndexSet<Rc<str>> {
        &self.0.environment
    }

    pub fn environment_mut(&mut self) -> &mut IndexSet<Rc<str>> {
        &mut self.0.environment
    }
}
