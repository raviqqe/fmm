#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum CallingConvention {
    Source,
    Tail,
    Target,
    Trampoline,
}
