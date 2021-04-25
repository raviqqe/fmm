#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Linkage {
    Global,
    WeakGlobal,
    Local,
}
