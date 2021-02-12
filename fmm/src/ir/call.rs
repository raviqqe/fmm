use super::calling_convention::CallingConvention;
use super::expression::Expression;
use crate::types;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct Call {
    type_: types::Function,
    function: Arc<Expression>,
    arguments: Vec<Expression>,
    calling_convention: CallingConvention,
    name: String,
}

impl Call {
    pub fn new(
        type_: types::Function,
        function: impl Into<Expression>,
        arguments: Vec<Expression>,
        calling_convention: CallingConvention,
        name: impl Into<String>,
    ) -> Self {
        Self {
            type_,
            function: function.into().into(),
            arguments,
            calling_convention,
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

    pub fn calling_convention(&self) -> CallingConvention {
        self.calling_convention
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
