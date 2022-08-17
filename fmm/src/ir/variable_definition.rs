use super::{expression::Expression, VariableDefinitionOptions};
use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct VariableDefinition {
    name: String,
    body: Expression,
    type_: Type,
    options: VariableDefinitionOptions,
}

impl VariableDefinition {
    pub fn new(
        name: impl Into<String>,
        body: impl Into<Expression>,
        type_: impl Into<Type>,
        options: VariableDefinitionOptions,
    ) -> Self {
        Self {
            name: name.into(),
            body: body.into(),
            type_: type_.into(),
            options,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn body(&self) -> &Expression {
        &self.body
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }

    pub fn options(&self) -> &VariableDefinitionOptions {
        &self.options
    }
}
