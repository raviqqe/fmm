use super::expression::Expression;
use crate::types::{self, Type};
use indexmap::IndexMap;

#[derive(Clone, Debug, PartialEq)]
pub struct Call(Box<CallInner>);

#[derive(Clone, Debug, PartialEq)]
struct CallInner {
    type_: types::Function,
    function: Expression,
    arguments: Vec<Expression>,
    name: String,
    environment: Option<IndexMap<String, Type>>,
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
                function: function.into(),
                arguments,
                name: name.into(),
                environment: None,
            }
            .into(),
        )
    }

    pub fn type_(&self) -> &types::Function {
        &self.0.type_
    }

    pub fn type_mut(&mut self) -> &mut types::Function {
        &mut self.0.type_
    }

    pub fn function(&self) -> &Expression {
        &self.0.function
    }

    pub fn function_mut(&mut self) -> &mut Expression {
        &mut self.0.function
    }

    pub fn arguments(&self) -> &[Expression] {
        &self.0.arguments
    }

    pub fn arguments_mut(&mut self) -> &mut Vec<Expression> {
        &mut self.0.arguments
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }

    pub fn name_mut(&mut self) -> &mut String {
        &mut self.0.name
    }

    pub fn environment(&self) -> Option<&IndexMap<String, Type>> {
        self.0.environment.as_ref()
    }

    pub fn environment_mut(&mut self) -> &mut Option<IndexMap<String, Type>> {
        &mut self.0.environment
    }
}
