use super::{block::Block, expression::Expression};
use crate::types::Type;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct If(Arc<IfInner>);

#[derive(Clone, Debug, PartialEq)]
struct IfInner {
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
        Self(
            IfInner {
                type_: type_.into(),
                condition: condition.into(),
                then,
                else_,
                name: name.into(),
            }
            .into(),
        )
    }

    pub fn type_(&self) -> &Type {
        &self.0.type_
    }

    pub fn condition(&self) -> &Expression {
        &self.0.condition
    }

    pub fn then(&self) -> &Block {
        &self.0.then
    }

    pub fn else_(&self) -> &Block {
        &self.0.else_
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }
}
