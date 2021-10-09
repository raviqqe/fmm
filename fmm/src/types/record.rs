use super::type_::Type;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Record {
    fields: Vec<Type>,
}

impl Record {
    pub const fn new(fields: Vec<Type>) -> Self {
        Self { fields }
    }

    pub fn fields(&self) -> &[Type] {
        &self.fields
    }
}
