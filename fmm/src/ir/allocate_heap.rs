use super::expression::Expression;

#[derive(Clone, Debug, PartialEq)]
pub struct AllocateHeap(Box<AllocateHeapInner>);

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

    pub fn size_mut(&mut self) -> &mut Expression {
        &mut self.0.size
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }
}
