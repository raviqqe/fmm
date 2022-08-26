use super::type_::Type;
use std::sync::Arc;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Record(Arc<RecordInner>);

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct RecordInner {
    fields: Vec<Type>,
}

impl Record {
    pub fn new(fields: Vec<Type>) -> Self {
        Self(RecordInner { fields }.into())
    }

    pub fn fields(&self) -> &[Type] {
        &self.0.fields
    }
}
