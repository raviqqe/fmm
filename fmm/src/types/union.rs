use super::type_::Type;
use std::sync::Arc;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Union {
    members: Arc<[Type]>,
}

impl Union {
    pub fn new(members: Vec<Type>) -> Self {
        Self {
            members: members.into(),
        }
    }

    pub fn members(&self) -> &[Type] {
        &self.members
    }
}
