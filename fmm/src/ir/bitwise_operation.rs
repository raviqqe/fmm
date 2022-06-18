use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BitwiseOperator {
    And,
    Or,
    Xor,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BitwiseOperation {
    type_: types::Primitive,
    operator: BitwiseOperator,
    lhs: Arc<Expression>,
    rhs: Arc<Expression>,
}

impl BitwiseOperation {
    pub fn new(
        type_: types::Primitive,
        operator: BitwiseOperator,
        lhs: impl Into<Expression>,
        rhs: impl Into<Expression>,
    ) -> Self {
        Self {
            type_,
            operator,
            lhs: Arc::new(lhs.into()),
            rhs: Arc::new(rhs.into()),
        }
    }

    pub fn type_(&self) -> types::Primitive {
        self.type_
    }

    pub fn operator(&self) -> BitwiseOperator {
        self.operator
    }

    pub fn lhs(&self) -> &Expression {
        &self.lhs
    }

    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }
}
