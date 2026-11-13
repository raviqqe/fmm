#[derive(Clone, Copy, Debug, Eq, PartialEq, Default)]
pub enum Linkage {
    #[default]
    External,
    Weak,
    Internal,
}
