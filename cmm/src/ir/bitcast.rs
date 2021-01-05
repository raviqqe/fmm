use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct Bitcast {
    from_type: types::Primitive,
    to_type: types::Primitive,
    expression: Arc<Expression>,
    name: String,
}

impl Bitcast {
    pub fn new(
        from_type: types::Primitive,
        to_type: types::Primitive,
        expression: impl Into<Expression>,
        name: impl Into<String>,
    ) -> Self {
        Self {
            from_type: from_type.into(),
            to_type: to_type.into(),
            expression: expression.into().into(),
            name: name.into(),
        }
    }

    pub fn from_type(&self) -> types::Primitive {
        self.from_type
    }

    pub fn to_type(&self) -> types::Primitive {
        self.to_type
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
