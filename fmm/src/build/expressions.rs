use super::contextful_expression::*;
use crate::ir::*;
use crate::types::Type;

pub fn variable(name: impl Into<String>, type_: impl Into<Type>) -> ContextfulExpression {
    ContextfulExpression::from_expression(Variable::new(name), type_)
}
