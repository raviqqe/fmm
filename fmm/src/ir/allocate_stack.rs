use crate::types::Type;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AllocateStack(Box<AllocateStackInner>);

#[derive(Clone, Debug, PartialEq, Eq)]
struct AllocateStackInner {
    type_: Type,
    name: Rc<str>,
}

impl AllocateStack {
    pub fn new(type_: impl Into<Type>, name: impl Into<Rc<str>>) -> Self {
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

    pub fn type_mut(&mut self) -> &mut Type {
        &mut self.0.type_
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }
}
