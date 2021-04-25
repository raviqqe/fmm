#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Linkage {
    External,
    Weak,
    Internal,
}

impl Default for Linkage {
    fn default() -> Self {
        Self::External
    }
}
