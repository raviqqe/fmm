use super::contextual_expression::*;
use crate::ir::*;
use crate::types::Type;

pub fn variable(name: impl Into<String>, type_: impl Into<Type>) -> ContextualExpression {
    ContextualExpression::from_expression(Variable::new(name), type_)
}
