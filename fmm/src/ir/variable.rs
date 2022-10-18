use std::sync::Arc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Variable {
    name: Arc<str>,
}

impl Variable {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into().into(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
