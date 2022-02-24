#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum CallingConvention {
    Source,
    Tail,
    Target,
    Trampoline,
}

impl CallingConvention {
    pub fn is_native(self) -> bool {
        matches!(self, Self::Tail | Self::Target)
    }
}
