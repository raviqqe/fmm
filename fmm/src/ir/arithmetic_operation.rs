use super::expression::Expression;
use crate::types;
use std::rc::Rc;

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
    lhs: Rc<Expression>,
    rhs: Rc<Expression>,
}

impl ArithmeticOperation {
    pub fn new(
        type_: types::Primitive,
        operator: ArithmeticOperator,
        lhs: impl Into<Expression>,
        rhs: impl Into<Expression>,
    ) -> Self {
        Self {
            type_,
            operator,
            lhs: Rc::new(lhs.into()),
            rhs: Rc::new(rhs.into()),
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
}
