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
pub struct ArithmeticOperation(Arc<ArithmeticOperationInner>);

#[derive(Debug, PartialEq)]
struct ArithmeticOperationInner {
    type_: types::Primitive,
    operator: ArithmeticOperator,
    lhs: Expression,
    rhs: Expression,
}

impl ArithmeticOperation {
    pub fn new(
        type_: types::Primitive,
        operator: ArithmeticOperator,
        lhs: impl Into<Expression>,
        rhs: impl Into<Expression>,
    ) -> Self {
        Self(
            ArithmeticOperationInner {
                type_,
                operator,
                lhs: lhs.into(),
                rhs: rhs.into(),
            }
            .into(),
        )
    }

    pub fn type_(&self) -> types::Primitive {
        self.0.type_
    }

    pub fn operator(&self) -> ArithmeticOperator {
        self.0.operator
    }

    pub fn lhs(&self) -> &Expression {
        &self.0.lhs
    }

    pub fn rhs(&self) -> &Expression {
        &self.0.rhs
    }
}
