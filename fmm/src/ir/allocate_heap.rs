use super::expression::Expression;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub struct AllocateHeap(Arc<AllocateHeapInner>);

#[derive(Clone, Debug, PartialEq)]
struct AllocateHeapInner {
    size: Expression,
    name: String,
}

impl AllocateHeap {
    pub fn new(size: impl Into<Expression>, name: impl Into<String>) -> Self {
        Self(
            AllocateHeapInner {
                size: size.into(),
                name: name.into(),
            }
            .into(),
        )
    }

    pub fn size(&self) -> &Expression {
        &self.0.size
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }
}
