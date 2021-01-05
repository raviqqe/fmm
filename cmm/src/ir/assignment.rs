use super::expression::Expression;
use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    type_: Type,
    expression: Expression,
    name: String,
}

impl Assignment {
    pub fn new(
        type_: impl Into<Type>,
        expression: impl Into<Expression>,
        name: impl Into<String>,
    ) -> Self {
        Self {
            type_: type_.into(),
            expression: expression.into(),
            name: name.into(),
        }
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
