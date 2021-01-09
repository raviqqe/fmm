use crate::ir::*;

// Do not derive Clone to guarantee that it's consumed only once!
pub struct ExpressionContext {
    instructions: Vec<Instruction>,
    expression: Expression,
}

impl ExpressionContext {
    pub fn new(
        instructions: impl IntoIterator<Item = Instruction>,
        expression: impl Into<Expression>,
    ) -> Self {
        Self {
            instructions: instructions.into_iter().collect(),
            expression: expression.into(),
        }
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }
}

impl<T: Into<Expression>> From<T> for ExpressionContext {
    fn from(expression: T) -> Self {
        Self::new(vec![], expression)
    }
}
