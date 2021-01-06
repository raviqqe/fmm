use super::argument::Argument;
use super::block::Block;
use crate::types::{self, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition {
    name: String,
    arguments: Vec<Argument>,
    body: Block,
    result_type: Type,
    type_: types::Function,
}

impl FunctionDefinition {
    pub fn new(
        name: impl Into<String>,
        arguments: Vec<Argument>,
        body: Block,
        result_type: impl Into<Type> + Clone,
    ) -> Self {
        Self {
            type_: types::Function::new(
                arguments
                    .iter()
                    .map(|argument| argument.type_().clone())
                    .collect(),
                result_type.clone(),
            ),
            name: name.into(),
            arguments,
            body,
            result_type: result_type.into(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn arguments(&self) -> &[Argument] {
        &self.arguments
    }

    pub fn body(&self) -> &Block {
        &self.body
    }

    pub fn result_type(&self) -> &Type {
        &self.result_type
    }

    pub fn type_(&self) -> &types::Function {
        &self.type_
    }
}
