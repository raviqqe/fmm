use super::linkage::Linkage;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VariableDefinitionOptions {
    address_named: bool,
    alignment: Option<usize>,
    linkage: Linkage,
    mutable: bool,
}

impl VariableDefinitionOptions {
    pub fn new() -> Self {
        Self {
            address_named: false,
            alignment: None,
            linkage: Linkage::External,
            mutable: true,
        }
    }

    pub fn alignment(&self) -> Option<usize> {
        self.alignment
    }

    pub fn linkage(&self) -> Linkage {
        self.linkage
    }

    pub fn is_address_named(&self) -> bool {
        self.address_named
    }

    pub fn is_mutable(&self) -> bool {
        self.mutable
    }

    pub fn set_address_named(self, address_named: bool) -> Self {
        Self {
            address_named,
            ..self
        }
    }

    pub fn set_alignment(self, alignment: Option<usize>) -> Self {
        Self { alignment, ..self }
    }

    pub fn set_linkage(self, linkage: Linkage) -> Self {
        Self { linkage, ..self }
    }

    pub fn set_mutable(self, mutable: bool) -> Self {
        Self { mutable, ..self }
    }
}

impl Default for VariableDefinitionOptions {
    fn default() -> Self {
        Self::new()
    }
}
