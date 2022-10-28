use crate::types::Type;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableDeclaration {
    name: Rc<str>,
    type_: Type,
}

impl VariableDeclaration {
    pub fn new(name: impl Into<Rc<str>>, type_: impl Into<Type>) -> Self {
        Self {
            name: name.into(),
            type_: type_.into(),
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
}
