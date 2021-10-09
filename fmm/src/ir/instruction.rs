use super::{
    allocate_heap::AllocateHeap, allocate_stack::AllocateStack, atomic_load::AtomicLoad,
    atomic_operation::AtomicOperation, atomic_store::AtomicStore, call::Call,
    compare_and_swap::CompareAndSwap, deconstruct_record::DeconstructRecord,
    deconstruct_union::DeconstructUnion, fence::Fence, free_heap::FreeHeap, if_::If, load::Load,
    pass_through::PassThrough, reallocate_heap::ReallocateHeap, store::Store,
};
use crate::types::{self, Type, GENERIC_POINTER_TYPE};

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
    PassThrough(PassThrough),
    ReallocateHeap(ReallocateHeap),
    Store(Store),
}

impl Instruction {
    pub fn name(&self) -> Option<&str> {
        match self {
            Self::AllocateHeap(allocate) => Some(allocate.name()),
            Self::AllocateStack(allocate) => Some(allocate.name()),
            Self::AtomicLoad(load) => Some(load.name()),
            Self::AtomicOperation(operation) => Some(operation.name()),
            Self::Call(call) => Some(call.name()),
            Self::CompareAndSwap(cas) => Some(cas.name()),
            Self::DeconstructRecord(deconstruct) => Some(deconstruct.name()),
            Self::DeconstructUnion(deconstruct) => Some(deconstruct.name()),
            Self::If(if_) => Some(if_.name()),
            Self::Load(load) => Some(load.name()),
            Self::PassThrough(pass) => Some(pass.name()),
            Self::ReallocateHeap(reallocate) => Some(reallocate.name()),
            Self::AtomicStore(_) | Self::Fence(_) | Self::FreeHeap(_) | Self::Store(_) => None,
        }
    }

    pub fn result_type(&self) -> Option<Type> {
        match self {
            Self::AllocateHeap(_) => Some(GENERIC_POINTER_TYPE.clone()),
            Self::AllocateStack(allocate) => {
                Some(types::Pointer::new(allocate.type_().clone()).into())
            }
            Self::AtomicLoad(load) => Some(load.type_().clone()),
            Self::AtomicOperation(operation) => Some(operation.type_().into()),
            Self::Call(call) => Some(call.type_().result().clone()),
            Self::CompareAndSwap(_) => Some(types::Primitive::Boolean.into()),
            Self::DeconstructRecord(deconstruct) => {
                Some(deconstruct.type_().fields()[deconstruct.element_index()].clone())
            }
            Self::DeconstructUnion(deconstruct) => {
                Some(deconstruct.type_().members()[deconstruct.member_index()].clone())
            }
            Self::If(if_) => Some(if_.type_().clone()),
            Self::Load(load) => Some(load.type_().clone()),
            Self::PassThrough(pass) => Some(pass.type_().clone()),
            Self::ReallocateHeap(_) => Some(GENERIC_POINTER_TYPE.clone()),
            Self::AtomicStore(_) | Self::Fence(_) | Self::FreeHeap(_) | Self::Store(_) => None,
        }
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

impl From<PassThrough> for Instruction {
    fn from(pass: PassThrough) -> Self {
        Self::PassThrough(pass)
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
