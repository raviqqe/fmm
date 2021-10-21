use super::type_::Type;

#[derive(Clone, Debug, Eq,  Ord, PartialEq, PartialOrd)]
pub struct Union {
    members: Vec<Type>,
}

impl Union {
    pub const fn new(members: Vec<Type>) -> Self {
        Self { members }
    }

    pub fn members(&self) -> &[Type] {
        &self.members
    }
}
