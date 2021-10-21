#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum CallingConvention {
    Source,
    Tail,
    Target,
}
