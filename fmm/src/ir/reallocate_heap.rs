use super::expression::Expression;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct ReallocateHeap(Rc<ReallocateHeapInner>);

#[derive(Clone, Debug, PartialEq)]
struct ReallocateHeapInner {
    pointer: Expression,
    size: Expression,
    name: String,
}

impl ReallocateHeap {
    pub fn new(
        pointer: impl Into<Expression>,
        size: impl Into<Expression>,
        name: impl Into<String>,
    ) -> Self {
        Self(
            ReallocateHeapInner {
                pointer: pointer.into(),
                size: size.into(),
                name: name.into(),
            }
            .into(),
        )
    }

    pub fn pointer(&self) -> &Expression {
        &self.0.pointer
    }

    pub fn size(&self) -> &Expression {
        &self.0.size
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }
}
