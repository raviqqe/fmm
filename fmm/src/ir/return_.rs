use super::expression::Expression;
use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct Return {
    type_: Type,
    expression: Expression,
}

impl Return {
    pub fn new(type_: impl Into<Type>, expression: impl Into<Expression>) -> Self {
        Self {
            type_: type_.into(),
            expression: expression.into(),
        }
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }
}
