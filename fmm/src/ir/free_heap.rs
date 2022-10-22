use super::expression::Expression;

#[derive(Clone, Debug, PartialEq)]
pub struct FreeHeap(Box<FreeHeapInner>);

#[derive(Clone, Debug, PartialEq)]
struct FreeHeapInner {
    pointer: Expression,
}

impl FreeHeap {
    pub fn new(pointer: impl Into<Expression>) -> Self {
        Self(
            FreeHeapInner {
                pointer: pointer.into(),
            }
            .into(),
        )
    }

    pub fn pointer(&self) -> &Expression {
        &self.0.pointer
    }

    pub fn pointer_mut(&mut self) -> &mut Expression {
        &mut self.0.pointer
    }
}
