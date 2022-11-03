use super::ArgumentOptions;
use crate::types::Type;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Argument {
    name: String,
    type_: Type,
    options: ArgumentOptions,
}

impl Argument {
    pub fn new(name: impl Into<String>, type_: impl Into<Type>) -> Self {
        Self::with_options(name, type_, Default::default())
    }

    pub fn with_options(
        name: impl Into<String>,
        type_: impl Into<Type>,
        options: ArgumentOptions,
    ) -> Self {
        Self {
            name: name.into(),
            type_: type_.into(),
            options,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }

    pub fn type_mut(&mut self) -> &mut Type {
        &mut self.type_
    }

    pub fn options(&self) -> &ArgumentOptions {
        &self.options
    }
}
