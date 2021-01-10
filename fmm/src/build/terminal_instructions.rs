use super::build_context::*;
use crate::ir::*;

pub fn branch(context: impl Into<BuildContext>) -> Block {
    let context = context.into();

    Block::new(
        context.instructions().iter().cloned(),
        Branch::new(context.type_().clone(), context.expression().clone()),
    )
}

pub fn return_(context: impl Into<BuildContext>) -> Block {
    let context = context.into();

    Block::new(
        context.instructions().iter().cloned(),
        Return::new(context.type_().clone(), context.expression().clone()),
    )
}

pub fn unreachable(context: impl Into<BuildContext>) -> Block {
    let context = context.into();

    Block::new(
        context.instructions().iter().cloned(),
        TerminalInstruction::Unreachable,
    )
}
