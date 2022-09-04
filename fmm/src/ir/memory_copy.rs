use super::expression::Expression;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct MemoryCopy {
    source: Arc<Expression>,
    destination: Arc<Expression>,
    size: Arc<Expression>,
}

impl MemoryCopy {
    pub fn new(
        source: impl Into<Expression>,
        destination: impl Into<Expression>,
        size: impl Into<Expression>,
    ) -> Self {
        Self {
            source: source.into().into(),
            destination: destination.into().into(),
            size: size.into().into(),
        }
    }

    pub fn source(&self) -> &Expression {
        &self.source
    }

    pub fn destination(&self) -> &Expression {
        &self.destination
    }

    pub fn size(&self) -> &Expression {
        &self.size
    }
}
