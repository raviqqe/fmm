use super::{
    allocate_heap::AllocateHeap, allocate_stack::AllocateStack, atomic_load::AtomicLoad,
    atomic_operation::AtomicOperation, atomic_store::AtomicStore, call::Call,
    compare_and_swap::CompareAndSwap, deconstruct_record::DeconstructRecord,
    deconstruct_union::DeconstructUnion, fence::Fence, free_heap::FreeHeap, if_::If, load::Load,
    reallocate_heap::ReallocateHeap, store::Store, MemoryCopy,
};
use crate::types::{self, generic_pointer_type, Type};

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    AllocateHeap(AllocateHeap),
    AllocateStack(AllocateStack),
    AtomicLoad(AtomicLoad),
    AtomicOperation(AtomicOperation),
    AtomicStore(AtomicStore),
    Call(Call),
    CompareAndSwap(CompareAndSwap),
    DeconstructRecord(DeconstructRecord),
    DeconstructUnion(DeconstructUnion),
    Fence(Fence),
    FreeHeap(FreeHeap),
    If(If),
    Load(Load),
    MemoryCopy(MemoryCopy),
    ReallocateHeap(ReallocateHeap),
    Store(Store),
}

impl Instruction {
    pub fn value(&self) -> Option<(&str, Type)> {
        match self {
            Self::AllocateHeap(allocate) => Some((allocate.name(), generic_pointer_type())),
            Self::AllocateStack(allocate) => Some((
                allocate.name(),
                types::Pointer::new(allocate.type_().clone()).into(),
            )),
            Self::AtomicLoad(load) => Some((load.name(), load.type_().clone())),
            Self::AtomicOperation(operation) => Some((operation.name(), operation.type_().into())),
            Self::Call(call) => Some((call.name(), call.type_().result().clone())),
            Self::CompareAndSwap(cas) => Some((cas.name(), types::Primitive::Boolean.into())),
            Self::DeconstructRecord(deconstruct) => Some((
                deconstruct.name(),
                deconstruct.type_().fields()[deconstruct.field_index()].clone(),
            )),
            Self::DeconstructUnion(deconstruct) => Some((
                deconstruct.name(),
                deconstruct.type_().members()[deconstruct.member_index()].clone(),
            )),
            Self::If(if_) => Some((if_.name(), if_.type_().clone())),
            Self::Load(load) => Some((load.name(), load.type_().clone())),
            Self::MemoryCopy(_) => None,
            Self::ReallocateHeap(reallocate) => Some((reallocate.name(), generic_pointer_type())),
            Self::AtomicStore(_) | Self::Fence(_) | Self::FreeHeap(_) | Self::Store(_) => None,
        }
    }

    pub fn has_blocks(&self) -> bool {
        matches!(self, Instruction::If(_))
    }
}

impl From<AllocateHeap> for Instruction {
    fn from(allocate: AllocateHeap) -> Self {
        Self::AllocateHeap(allocate)
    }
}

impl From<AllocateStack> for Instruction {
    fn from(allocate: AllocateStack) -> Self {
        Self::AllocateStack(allocate)
    }
}

impl From<AtomicLoad> for Instruction {
    fn from(load: AtomicLoad) -> Self {
        Self::AtomicLoad(load)
    }
}

impl From<AtomicOperation> for Instruction {
    fn from(operation: AtomicOperation) -> Self {
        Self::AtomicOperation(operation)
    }
}

impl From<AtomicStore> for Instruction {
    fn from(store: AtomicStore) -> Self {
        Self::AtomicStore(store)
    }
}

impl From<Call> for Instruction {
    fn from(call: Call) -> Self {
        Self::Call(call)
    }
}

impl From<CompareAndSwap> for Instruction {
    fn from(compare_and_swap: CompareAndSwap) -> Self {
        Self::CompareAndSwap(compare_and_swap)
    }
}

impl From<DeconstructRecord> for Instruction {
    fn from(deconstruct: DeconstructRecord) -> Self {
        Self::DeconstructRecord(deconstruct)
    }
}

impl From<DeconstructUnion> for Instruction {
    fn from(deconstruct: DeconstructUnion) -> Self {
        Self::DeconstructUnion(deconstruct)
    }
}

impl From<Fence> for Instruction {
    fn from(fence: Fence) -> Self {
        Self::Fence(fence)
    }
}

impl From<FreeHeap> for Instruction {
    fn from(free: FreeHeap) -> Self {
        Self::FreeHeap(free)
    }
}

impl From<If> for Instruction {
    fn from(if_: If) -> Self {
        Self::If(if_)
    }
}

impl From<Load> for Instruction {
    fn from(load: Load) -> Self {
        Self::Load(load)
    }
}

impl From<MemoryCopy> for Instruction {
    fn from(copy: MemoryCopy) -> Self {
        Self::MemoryCopy(copy)
    }
}

impl From<ReallocateHeap> for Instruction {
    fn from(reallocate: ReallocateHeap) -> Self {
        Self::ReallocateHeap(reallocate)
    }
}

impl From<Store> for Instruction {
    fn from(store: Store) -> Self {
        Self::Store(store)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::size_of;

    #[test]
    fn type_size() {
        assert!(size_of::<Type>() <= 2 * size_of::<usize>());
    }
}
