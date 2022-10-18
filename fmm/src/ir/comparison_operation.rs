use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ComparisonOperator {
    Equal,
    NotEqual,
    LessThan(bool),
    GreaterThan(bool),
    LessThanOrEqual(bool),
    GreaterThanOrEqual(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ComparisonOperation(Arc<ComparisonOperationInner>);

#[derive(Clone, Debug, PartialEq)]
struct ComparisonOperationInner {
    type_: types::Primitive,
    operator: ComparisonOperator,
    lhs: Expression,
    rhs: Expression,
}

impl ComparisonOperation {
    pub const RESULT_TYPE: types::Primitive = types::Primitive::Boolean;

    pub fn new(
        type_: types::Primitive,
        operator: ComparisonOperator,
        lhs: impl Into<Expression>,
        rhs: impl Into<Expression>,
    ) -> Self {
        Self(
            ComparisonOperationInner {
                type_,
                operator,
                lhs: (lhs.into()),
                rhs: (rhs.into()),
            }
            .into(),
        )
    }

    pub fn type_(&self) -> types::Primitive {
        self.0.type_
    }

    pub fn operator(&self) -> ComparisonOperator {
        self.0.operator
    }

    pub fn lhs(&self) -> &Expression {
        &self.0.lhs
    }

    pub fn rhs(&self) -> &Expression {
        &self.0.rhs
    }
}
