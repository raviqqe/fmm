use super::{expression::Expression, linkage::Linkage};
use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct VariableDefinition {
    name: String,
    body: Expression,
    type_: Type,
    mutable: bool,
    linkage: Linkage,
    alignment: Option<usize>,
}

impl VariableDefinition {
    pub fn new(
        name: impl Into<String>,
        body: impl Into<Expression>,
        type_: impl Into<Type>,
        mutable: bool,
        linkage: Linkage,
        alignment: impl Into<Option<usize>>,
    ) -> Self {
        Self {
            name: name.into(),
            body: body.into(),
            type_: type_.into(),
            mutable,
            linkage,
            alignment: alignment.into(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn body(&self) -> &Expression {
        &self.body
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }

    pub fn is_mutable(&self) -> bool {
        self.mutable
    }

    pub fn linkage(&self) -> Linkage {
        self.linkage
    }

    pub fn alignment(&self) -> Option<usize> {
        self.alignment
    }
}
