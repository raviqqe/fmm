use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArithmeticOperation {
    type_: types::Primitive,
    operator: ArithmeticOperator,
    lhs: Arc<Expression>,
    rhs: Arc<Expression>,
    name: String,
}

impl ArithmeticOperation {
    pub fn new(
        type_: types::Primitive,
        operator: ArithmeticOperator,
        lhs: impl Into<Expression>,
        rhs: impl Into<Expression>,
        name: impl Into<String>,
    ) -> Self {
        Self {
            type_,
            operator,
            lhs: Arc::new(lhs.into()),
            rhs: Arc::new(rhs.into()),
            name: name.into(),
        }
    }

    pub fn type_(&self) -> types::Primitive {
        self.type_
    }

    pub fn operator(&self) -> ArithmeticOperator {
        self.operator
    }

    pub fn lhs(&self) -> &Expression {
        &self.lhs
    }

    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
