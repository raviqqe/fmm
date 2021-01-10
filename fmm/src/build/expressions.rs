use super::build_context::*;
use crate::ir::*;
use crate::types::Type;

pub fn variable(name: impl Into<String>, type_: impl Into<Type>) -> BuildContext {
    BuildContext::from_expression(Variable::new(name), type_)
}
