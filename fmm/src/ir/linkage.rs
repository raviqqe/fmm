#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[derive(Default)]
pub enum Linkage {
    #[default]
    External,
    Weak,
    Internal,
}

