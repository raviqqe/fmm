use super::expression::Expression;

#[derive(Clone, Debug, PartialEq)]
pub struct ReallocateHeap {
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
        Self {
            pointer: pointer.into(),
            size: size.into(),
            name: name.into(),
        }
    }

    pub fn pointer(&self) -> &Expression {
        &self.pointer
    }

    pub fn size(&self) -> &Expression {
        &self.size
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
