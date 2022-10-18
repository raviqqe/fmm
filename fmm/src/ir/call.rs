use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct Call(Arc<CallInner>);

#[derive(Clone, Debug, PartialEq)]
struct CallInner {
    type_: types::Function,
    function: Expression,
    arguments: Vec<Expression>,
    name: String,
}

impl Call {
    pub fn new(
        type_: types::Function,
        function: impl Into<Expression>,
        arguments: Vec<Expression>,
        name: impl Into<String>,
    ) -> Self {
        Self(
            CallInner {
                type_,
                function: function.into().into(),
                arguments: arguments.into(),
                name: name.into(),
            }
            .into(),
        )
    }

    pub fn type_(&self) -> &types::Function {
        &self.0.type_
    }

    pub fn function(&self) -> &Expression {
        &self.0.function
    }

    pub fn arguments(&self) -> &[Expression] {
        &self.0.arguments
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }
}
