use super::expression::Expression;
use super::instruction::Instruction;

#[derive(Clone, Debug, PartialEq)]
pub struct Alternative {
    condition: Expression,
    instructions: Vec<Instruction>,
    result: Expression,
}

impl Alternative {
    pub fn new(
        condition: impl Into<Expression>,
        instructions: Vec<Instruction>,
        result: impl Into<Expression>,
    ) -> Self {
        Self {
            condition: condition.into(),
            instructions,
            result: result.into(),
        }
    }

    pub fn condition(&self) -> &Expression {
        &self.condition
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub fn result(&self) -> &Expression {
        &self.result
    }
}
