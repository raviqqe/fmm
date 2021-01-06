use super::block::Block;
use super::expression::Expression;
use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    type_: Type,
    condition: Expression,
    then: Block,
    else_: Block,
    name: String,
}

impl If {
    pub fn new(
        type_: impl Into<Type>,
        condition: impl Into<Expression>,
        then: Block,
        else_: Block,
        name: impl Into<String>,
    ) -> Self {
        Self {
            type_: type_.into(),
            condition: condition.into(),
            then,
            else_,
            name: name.into(),
        }
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }

    pub fn condition(&self) -> &Expression {
        &self.condition
    }

    pub fn then(&self) -> &Block {
        &self.then
    }

    pub fn else_(&self) -> &Block {
        &self.else_
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
