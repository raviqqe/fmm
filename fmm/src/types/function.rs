use super::{calling_convention::CallingConvention, type_::Type};
use std::sync::Arc;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Function(Arc<FunctionInner>);

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct FunctionInner {
    arguments: Vec<Type>,
    result: Arc<Type>,
    calling_convention: CallingConvention,
}

impl Function {
    pub fn new(
        arguments: Vec<Type>,
        result: impl Into<Type>,
        calling_convention: CallingConvention,
    ) -> Self {
        Self(
            FunctionInner {
                arguments,
                result: result.into().into(),
                calling_convention,
            }
            .into(),
        )
    }

    pub fn arguments(&self) -> &[Type] {
        &self.0.arguments
    }

    pub fn result(&self) -> &Type {
        &self.0.result
    }

    pub fn calling_convention(&self) -> CallingConvention {
        self.0.calling_convention
    }
}
