use super::linkage::Linkage;
use crate::types::CallingConvention;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionDefinitionOptions {
    address_named: bool,
    calling_convention: CallingConvention,
    inline: bool,
    linkage: Linkage,
}

impl FunctionDefinitionOptions {
    pub fn new() -> Self {
        Self {
            address_named: true,
            calling_convention: CallingConvention::Source,
            inline: false,
            linkage: Linkage::External,
        }
    }

    pub fn calling_convention(&self) -> CallingConvention {
        self.calling_convention
    }

    pub fn is_inlined(&self) -> bool {
        self.inline
    }

    pub fn is_address_named(&self) -> bool {
        self.address_named
    }

    pub fn linkage(&self) -> Linkage {
        self.linkage
    }

    pub fn set_address_named(self, address_named: bool) -> Self {
        Self {
            address_named,
            ..self
        }
    }

    pub fn set_calling_convention(self, calling_convention: CallingConvention) -> Self {
        Self {
            calling_convention,
            ..self
        }
    }

    pub fn set_inline(self, inline: bool) -> Self {
        Self { inline, ..self }
    }

    pub fn set_linkage(self, linkage: Linkage) -> Self {
        Self { linkage, ..self }
    }
}

impl Default for FunctionDefinitionOptions {
    fn default() -> Self {
        Self::new()
    }
}
