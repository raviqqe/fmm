use crate::types::Type;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AllocateStack(Rc<AllocateStackInner>);

#[derive(Clone, Debug, PartialEq, Eq)]
struct AllocateStackInner {
    type_: Type,
    name: String,
}

impl AllocateStack {
    pub fn new(type_: impl Into<Type>, name: impl Into<String>) -> Self {
        Self(
            AllocateStackInner {
                type_: type_.into(),
                name: name.into(),
            }
            .into(),
        )
    }

    pub fn type_(&self) -> &Type {
        &self.0.type_
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }
}
