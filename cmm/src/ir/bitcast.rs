use super::expression::Expression;
use crate::types::Type;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct Bitcast {
    from: Type,
    to: Type,
    expression: Arc<Expression>,
    name: String,
}

impl Bitcast {
    pub fn new(
        from: impl Into<Type>,
        to: impl Into<Type>,
        expression: impl Into<Expression>,
        name: impl Into<String>,
    ) -> Self {
        Self {
            from: from.into(),
            to: to.into(),
            expression: expression.into().into(),
            name: name.into(),
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

    pub fn name(&self) -> &str {
        &self.name
    }
}
