use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct AlignOf {
    type_: Type,
}

impl AlignOf {
    pub fn new(type_: impl Into<Type>) -> Self {
        Self {
            type_: type_.into(),
        }
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }
}
