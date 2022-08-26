use super::{calling_convention::CallingConvention, type_::Type};
use std::sync::Arc;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Function {
    arguments: Arc<[Type]>,
    result: Arc<Type>,
    calling_convention: CallingConvention,
}

impl Function {
    pub fn new(
        arguments: Vec<Type>,
        result: impl Into<Type>,
        calling_convention: CallingConvention,
    ) -> Self {
        Self {
            arguments: arguments.into(),
            result: result.into().into(),
            calling_convention,
        }
    }

    pub fn arguments(&self) -> &[Type] {
        &self.arguments
    }

    pub fn result(&self) -> &Type {
        &self.result
    }

    pub fn calling_convention(&self) -> CallingConvention {
        self.calling_convention
    }
}
