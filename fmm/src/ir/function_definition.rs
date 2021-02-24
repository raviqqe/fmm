use super::argument::Argument;
use super::block::Block;
use crate::types::{self, CallingConvention, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition {
    name: String,
    arguments: Vec<Argument>,
    body: Block,
    result_type: Type,
    type_: types::Function,
    global: bool,
}

impl FunctionDefinition {
    pub fn new(
        name: impl Into<String>,
        arguments: Vec<Argument>,
        body: Block,
        result_type: impl Into<Type>,
        calling_convention: CallingConvention,
        global: bool,
    ) -> Self {
        let result_type = result_type.into();

        Self {
            type_: types::Function::new(
                arguments
                    .iter()
                    .map(|argument| argument.type_().clone())
                    .collect(),
                result_type.clone(),
                calling_convention,
            ),
            name: name.into(),
            arguments,
            body,
            result_type,
            global,
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

    pub fn calling_convention(&self) -> CallingConvention {
        self.type_.calling_convention()
    }

    pub fn is_global(&self) -> bool {
        self.global
    }
}
