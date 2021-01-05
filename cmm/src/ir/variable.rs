#[derive(Clone, Debug, PartialEq)]
pub struct Variable {
    id: u64,
}

impl Variable {
    pub fn new(id: u64) -> Self {
        Self { id }
    }

    pub fn id(&self) -> u64 {
        self.id
    }
}
