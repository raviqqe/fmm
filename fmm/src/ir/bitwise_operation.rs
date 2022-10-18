use super::expression::Expression;
use crate::types;
use std::rc::Rc;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BitwiseOperator {
    And,
    Or,
    Xor,
    LeftShift,
    RightShift(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub struct BitwiseOperation(Rc<BitwiseOperationInner>);

#[derive(Clone, Debug, PartialEq)]
struct BitwiseOperationInner {
    type_: types::Primitive,
    operator: BitwiseOperator,
    lhs: Expression,
    rhs: Expression,
}

impl BitwiseOperation {
    pub fn new(
        type_: types::Primitive,
        operator: BitwiseOperator,
        lhs: impl Into<Expression>,
        rhs: impl Into<Expression>,
    ) -> Self {
        Self(
            BitwiseOperationInner {
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

    pub fn operator(&self) -> BitwiseOperator {
        self.0.operator
    }

    pub fn lhs(&self) -> &Expression {
        &self.0.lhs
    }

    pub fn rhs(&self) -> &Expression {
        &self.0.rhs
    }
}
