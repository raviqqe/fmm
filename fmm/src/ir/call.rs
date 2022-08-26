use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct Call {
    type_: types::Function,
    function: Arc<Expression>,
    arguments: Arc<Vec<Expression>>,
    name: String,
}

impl Call {
    pub fn new(
        type_: types::Function,
        function: impl Into<Expression>,
        arguments: Vec<Expression>,
        name: impl Into<String>,
    ) -> Self {
        Self {
            type_,
            function: function.into().into(),
            arguments: arguments.into(),
            name: name.into(),
        }
    }

    pub fn type_(&self) -> &types::Function {
        &self.type_
    }

    pub fn function(&self) -> &Expression {
        &self.function
    }

    pub fn arguments(&self) -> &[Expression] {
        &self.arguments
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
