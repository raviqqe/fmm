use crate::types;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionDeclaration {
    name: Rc<str>,
    type_: types::Function,
}

impl FunctionDeclaration {
    pub fn new(name: impl Into<Rc<str>>, type_: types::Function) -> Self {
        Self {
            name: name.into(),
            type_,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn type_(&self) -> &types::Function {
        &self.type_
    }

    pub fn type_mut(&mut self) -> &mut types::Function {
        &mut self.type_
    }
}
