use super::type_::Type;
use std::sync::Arc;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Record {
    fields: Arc<[Type]>,
}

impl Record {
    pub fn new(fields: Vec<Type>) -> Self {
        Self {
            fields: fields.into(),
        }
    }

    pub fn fields(&self) -> &[Type] {
        &self.fields
    }
}
