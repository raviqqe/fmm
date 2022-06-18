use super::expression::Expression;
use crate::types;
use std::rc::Rc;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ComparisonOperator {
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ComparisonOperation {
    type_: types::Primitive,
    operator: ComparisonOperator,
    lhs: Rc<Expression>,
    rhs: Rc<Expression>,
}

impl ComparisonOperation {
    pub const RESULT_TYPE: types::Primitive = types::Primitive::Boolean;

    pub fn new(
        type_: types::Primitive,
        operator: ComparisonOperator,
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

    pub fn operator(&self) -> ComparisonOperator {
        self.operator
    }

    pub fn lhs(&self) -> &Expression {
        &self.lhs
    }

    pub fn rhs(&self) -> &Expression {
        &self.rhs
    }
}
