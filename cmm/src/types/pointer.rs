use super::type_::Type;
use std::sync::Arc;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Pointer {
    type_: Arc<Type>,
}

impl Pointer {
    pub fn new(type_: impl Into<Type>) -> Self {
        Self {
            type_: type_.into().into(),
        }
    }

    pub fn element(&self) -> &Type {
        &self.type_
    }
}
