use super::type_::Type;
use std::sync::Arc;

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Pointer {
    element: Arc<Type>,
}

impl Pointer {
    pub fn new(element: impl Into<Type>) -> Self {
        Self {
            element: element.into().into(),
        }
    }

    pub fn element(&self) -> &Type {
        &self.element
    }
}
