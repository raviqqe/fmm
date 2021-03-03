use super::size_of::SizeOf;
use crate::types::{self, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct AlignOf {
    type_: Type,
}

impl AlignOf {
    pub const RESULT_TYPE: types::Primitive = SizeOf::RESULT_TYPE;

    pub fn new(type_: impl Into<Type>) -> Self {
        Self {
            type_: type_.into(),
        }
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }
}
