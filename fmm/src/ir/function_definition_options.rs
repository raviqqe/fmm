use super::linkage::Linkage;
use crate::types::CallingConvention;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionDefinitionOptions {
    calling_convention: CallingConvention,
    linkage: Linkage,
    address_named: bool,
}

impl FunctionDefinitionOptions {
    pub fn new() -> Self {
        Self {
            calling_convention: CallingConvention::Source,
            linkage: Linkage::External,
            address_named: false,
        }
    }

    pub fn calling_convention(&self) -> CallingConvention {
        self.calling_convention
    }

    pub fn linkage(&self) -> Linkage {
        self.linkage
    }

    pub fn is_address_named(&self) -> bool {
        self.address_named
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

    pub fn set_address_named(self, address_named: bool) -> Self {
        Self {
            address_named,
            ..self
        }
    }
}

impl Default for FunctionDefinitionOptions {
    fn default() -> Self {
        Self::new()
    }
}
