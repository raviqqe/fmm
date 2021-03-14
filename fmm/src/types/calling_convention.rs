#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CallingConvention {
    Source,
    Tail,
    Target,
}
