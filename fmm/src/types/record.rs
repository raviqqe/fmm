use super::type_::Type;
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    sync::Arc,
};

#[derive(Clone, Debug, Eq, Ord, PartialOrd)]
pub struct Record(Arc<RecordInner>);

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
struct RecordInner {
    fields: Vec<Type>,
    hash: u64, // cached hash
}

impl Record {
    pub fn new(fields: Vec<Type>) -> Self {
        let mut hasher = DefaultHasher::new();

        fields.hash(&mut hasher);

        Self(
            RecordInner {
                fields,
                hash: hasher.finish(),
            }
            .into(),
        )
    }

    pub fn fields(&self) -> &[Type] {
        &self.0.fields
    }
}

impl PartialEq for Record {
    fn eq(&self, other: &Self) -> bool {
        &self.0.fields == &other.0.fields
    }
}

impl Hash for Record {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.0.hash.hash(hasher);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Primitive;

    fn hash(value: &impl Hash) -> u64 {
        let mut hasher = DefaultHasher::new();

        value.hash(&mut hasher);

        hasher.finish()
    }

    #[test]
    fn equal_field() {
        let record = Record::new(vec![Primitive::PointerInteger.into()]);

        assert_eq!(&record, &record);
        assert_ne!(record, Record::new(vec![]));
    }

    #[test]
    fn hash_field() {
        let record = Record::new(vec![Primitive::PointerInteger.into()]);

        assert_eq!(hash(&record), hash(&record));
        assert_ne!(hash(&record), hash(&Record::new(vec![])));
    }
}
