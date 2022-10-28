use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable {
    name: Rc<str>,
}

impl Variable {
    pub fn new(name: impl Into<Rc<str>>) -> Self {
        Self {
            name: name.into().into(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
