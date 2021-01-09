mod instruction_context;
mod names;

use crate::ir::*;
use crate::types::Type;
use instruction_context::InstructionContext;
pub use instruction_context::*;
use names::*;

pub fn atomic_load(type_: impl Into<Type>, pointer: InstructionContext) -> InstructionContext {
    let name = generate_name();

    InstructionContext::new(
        pointer
            .instructions()
            .iter()
            .cloned()
            .chain(vec![AtomicLoad::new(
                type_,
                pointer.expression().clone(),
                &name,
            )
            .into()]),
        Variable::new(name),
    )
}
