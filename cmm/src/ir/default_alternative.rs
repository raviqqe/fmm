use super::expression::Expression;
use super::instruction::Instruction;

#[derive(Clone, Debug, PartialEq)]
pub struct DefaultAlternative {
    instructions: Vec<Instruction>,
    result: Expression,
}

impl DefaultAlternative {
    pub fn new(instructions: Vec<Instruction>, result: impl Into<Expression>) -> Self {
        Self {
            instructions,
            result: result.into(),
        }
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub fn result(&self) -> &Expression {
        &self.result
    }
}
