use super::linkage::Linkage;
use crate::types::CallingConvention;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionDefinitionOptions {
    calling_convention: CallingConvention,
    linkage: Linkage,
    named_address: bool,
}

impl FunctionDefinitionOptions {
    pub fn new() -> Self {
        Self {
            calling_convention: CallingConvention::Source,
            linkage: Linkage::External,
            named_address: false,
        }
    }

    pub fn calling_convention(&self) -> CallingConvention {
        self.calling_convention
    }

    pub fn linkage(&self) -> Linkage {
        self.linkage
    }

    pub fn named_address(&self) -> bool {
        self.named_address
    }

    pub fn set_calling_convention(self, calling_convention: CallingConvention) -> Self {
        Self {
            calling_convention,
            ..self
        }
    }

    pub fn set_linkage(self, linkage: Linkage) -> Self {
        Self { linkage, ..self }
    }

    pub fn set_named_address(self, named_address: bool) -> Self {
        Self {
            named_address,
            ..self
        }
    }
}
