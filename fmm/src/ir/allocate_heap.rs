use super::expression::Expression;

#[derive(Clone, Debug, PartialEq)]
pub struct AllocateHeap {
    size: Expression,
    name: String,
}

impl AllocateHeap {
    pub fn new(size: impl Into<Expression>, name: impl Into<String>) -> Self {
        Self {
            size: size.into(),
            name: name.into(),
        }
    }

    pub fn size(&self) -> &Expression {
        &self.size
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
