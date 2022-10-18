use crate::{ir::Expression, types::Type};
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct BitCast(Arc<BitCastInner>);

#[derive(Clone, Debug, PartialEq)]
struct BitCastInner {
    from: Type,
    to: Type,
    expression: Expression,
}

impl BitCast {
    pub fn new(
        from: impl Into<Type>,
        to: impl Into<Type>,
        expression: impl Into<Expression>,
    ) -> Self {
        Self(
            BitCastInner {
                from: from.into(),
                to: to.into(),
                expression: expression.into(),
            }
            .into(),
        )
    }

    pub fn from(&self) -> &Type {
        &self.0.from
    }

    pub fn to(&self) -> &Type {
        &self.0.to
    }

    pub fn expression(&self) -> &Expression {
        &self.0.expression
    }
}
