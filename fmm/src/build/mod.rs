mod instruction_context;
mod names;

use crate::ir::*;
use crate::types::{self, Type};
use instruction_context::InstructionContext;
pub use instruction_context::*;
use names::*;

pub fn atomic_load(
    type_: impl Into<Type>,
    pointer: impl Into<InstructionContext>,
) -> InstructionContext {
    let pointer = pointer.into();
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

pub fn load(type_: impl Into<Type>, pointer: impl Into<InstructionContext>) -> InstructionContext {
    let pointer = pointer.into();
    let name = generate_name();

    InstructionContext::new(
        pointer.instructions().iter().cloned().chain(vec![Load::new(
            type_,
            pointer.expression().clone(),
            &name,
        )
        .into()]),
        Variable::new(name),
    )
}

pub fn pointer_address(
    type_: types::Pointer,
    pointer: impl Into<InstructionContext>,
    offset: impl Into<InstructionContext>,
) -> InstructionContext {
    let pointer = pointer.into();
    let offset = offset.into();
    let name = generate_name();

    InstructionContext::new(
        pointer
            .instructions()
            .iter()
            .chain(offset.instructions())
            .cloned()
            .chain(vec![PointerAddress::new(
                type_,
                pointer.expression().clone(),
                offset.expression().clone(),
                &name,
            )
            .into()]),
        Variable::new(name),
    )
}

pub fn record_address(
    type_: types::Record,
    pointer: impl Into<InstructionContext>,
    element_index: usize,
) -> InstructionContext {
    let pointer = pointer.into();
    let name = generate_name();

    InstructionContext::new(
        pointer
            .instructions()
            .iter()
            .cloned()
            .chain(vec![RecordAddress::new(
                type_,
                pointer.expression().clone(),
                element_index,
                &name,
            )
            .into()]),
        Variable::new(name),
    )
}

pub fn union_address(
    type_: types::Union,
    pointer: impl Into<InstructionContext>,
    member_index: usize,
) -> InstructionContext {
    let pointer = pointer.into();
    let name = generate_name();

    InstructionContext::new(
        pointer
            .instructions()
            .iter()
            .cloned()
            .chain(vec![UnionAddress::new(
                type_,
                pointer.expression().clone(),
                member_index,
                &name,
            )
            .into()]),
        Variable::new(name),
    )
}
