use super::type_::Type;
use std::rc::Rc;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Pointer {
    element: Rc<Type>,
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
