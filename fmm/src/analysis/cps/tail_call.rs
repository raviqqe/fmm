use crate::ir::*;

pub fn is_tail_call(instructions: &[Instruction]) -> bool {
    instructions
        .iter()
        .all(|instruction| matches!(instruction, Instruction::PassThrough(_)))
}
