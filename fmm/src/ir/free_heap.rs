use super::expression::Expression;

#[derive(Clone, Debug, PartialEq)]
pub struct FreeHeap {
    pointer: Expression,
}

impl FreeHeap {
    pub fn new(pointer: impl Into<Expression>) -> Self {
        Self {
            pointer: pointer.into(),
        }
    }

    pub fn pointer(&self) -> &Expression {
        &self.pointer
    }

    pub fn pointer_mut(&mut self) -> &mut Expression {
        &mut self.pointer
    }
}
