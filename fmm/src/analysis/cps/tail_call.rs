use crate::ir::*;

// TODO Consider removing or canonicalizing pass-through instructions.
pub fn is_tail_call(instructions: &[Instruction]) -> bool {
    instructions
        .iter()
        .all(|instruction| matches!(instruction, Instruction::PassThrough(_)))
}
