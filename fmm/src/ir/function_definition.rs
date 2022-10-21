use super::{argument::Argument, block::Block, FunctionDefinitionOptions};
use crate::types::{self, Type};
use std::cell::RefCell;

#[derive(Clone, Debug)]
pub struct FunctionDefinition {
    name: String,
    arguments: Vec<Argument>,
    body: Block,
    result_type: Type,
    type_: RefCell<Option<types::Function>>,
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
            type_: None.into(),
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

    pub fn type_(&self) -> types::Function {
        if let Some(type_) = &*self.type_.borrow() {
            return type_.clone();
        }

        *self.type_.borrow_mut() = Some(types::Function::new(
            self.arguments
                .iter()
                .map(|argument| argument.type_().clone())
                .collect(),
            self.result_type.clone(),
            self.options.calling_convention(),
        ));

        self.type_()
    }

    pub fn options(&self) -> &FunctionDefinitionOptions {
        &self.options
    }

    pub fn arguments_mut(&mut self) -> &mut [Argument] {
        *self.type_.borrow_mut() = None;

        &mut self.arguments
    }

    pub fn body_mut(&mut self) -> &mut Block {
        &mut self.body
    }

    pub fn result_type_mut(&mut self) -> &mut Type {
        *self.type_.borrow_mut() = None;

        &mut self.result_type
    }
}

impl PartialEq for FunctionDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.arguments == other.arguments
            && self.body == other.body
            && self.result_type == other.result_type
            && self.options == other.options
    }
}
