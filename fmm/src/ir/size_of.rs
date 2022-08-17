use crate::types::{self, Type};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SizeOf {
    type_: Type,
}

impl SizeOf {
    pub const RESULT_TYPE: types::Primitive = types::Primitive::PointerInteger;

    pub fn new(type_: impl Into<Type>) -> Self {
        Self {
            type_: type_.into(),
        }
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }
}
