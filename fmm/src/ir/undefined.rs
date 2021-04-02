use crate::types::Type;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Undefined {
    type_: Type,
}

impl Undefined {
    pub fn new(type_: impl Into<Type>) -> Self {
        Self {
            type_: type_.into(),
        }
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }
}
