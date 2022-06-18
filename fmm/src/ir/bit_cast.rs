use crate::{ir::Expression, types::Type};
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct BitCast {
    from: Type,
    to: Type,
    expression: Arc<Expression>,
}

impl BitCast {
    pub fn new(
        from: impl Into<Type>,
        to: impl Into<Type>,
        expression: impl Into<Expression>,
    ) -> Self {
        Self {
            from: from.into(),
            to: to.into(),
            expression: expression.into().into(),
        }
    }

    pub fn from(&self) -> &Type {
        &self.from
    }

    pub fn to(&self) -> &Type {
        &self.to
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }
}
