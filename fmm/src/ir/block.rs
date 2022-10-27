use super::{instruction::Instruction, terminal_instruction::TerminalInstruction};

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    instructions: Vec<Instruction>,
    terminal_instruction: TerminalInstruction,
}

impl Block {
    pub fn new(
        instructions: Vec<Instruction>,
        terminal_instruction: impl Into<TerminalInstruction>,
    ) -> Self {
        Self {
            instructions,
            terminal_instruction: terminal_instruction.into(),
        }
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub fn terminal_instruction(&self) -> &TerminalInstruction {
        &self.terminal_instruction
    }

    pub fn instructions_mut(&mut self) -> &mut Vec<Instruction> {
        &mut self.instructions
    }

    pub fn terminal_instruction_mut(&mut self) -> &mut TerminalInstruction {
        &mut self.terminal_instruction
    }
}
