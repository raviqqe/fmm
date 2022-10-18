use super::{argument::Argument, block::Block, FunctionDefinitionOptions};
use crate::types::{self, Type};
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition {
    name: String,
    arguments: Rc<Vec<Argument>>,
    body: Block,
    result_type: Type,
    type_: types::Function,
    options: FunctionDefinitionOptions,
}

impl FunctionDefinition {
    pub fn new(
        name: impl Into<String>,
        arguments: Vec<Argument>,
        result_type: impl Into<Type>,
        body: Block,
        options: FunctionDefinitionOptions,
    ) -> Self {
        let result_type = result_type.into();

        Self {
            type_: types::Function::new(
                arguments
                    .iter()
                    .map(|argument| argument.type_().clone())
                    .collect(),
                result_type.clone(),
                options.calling_convention(),
            ),
            name: name.into(),
            arguments: arguments.into(),
            result_type,
            body,
            options,
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

    pub fn options(&self) -> &FunctionDefinitionOptions {
        &self.options
    }
}
