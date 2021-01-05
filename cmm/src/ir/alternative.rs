use super::expression::Expression;
use super::instruction::Instruction;
use super::primitive::Primitive;

#[derive(Clone, Debug, PartialEq)]
pub struct Alternative {
    condition: Primitive,
    instructions: Vec<Instruction>,
    result: Expression,
}

impl Alternative {
    pub fn new(
        condition: impl Into<Primitive>,
        instructions: Vec<Instruction>,
        result: impl Into<Expression>,
    ) -> Self {
        Self {
            condition: condition.into(),
            instructions,
            result: result.into(),
        }
    }

    pub fn condition(&self) -> Primitive {
        self.condition
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub fn result(&self) -> &Expression {
        &self.result
    }
}
