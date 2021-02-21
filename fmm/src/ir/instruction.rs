use super::allocate_heap::AllocateHeap;
use super::allocate_stack::AllocateStack;
use super::arithmetic_operation::ArithmeticOperation;
use super::atomic_load::AtomicLoad;
use super::atomic_store::AtomicStore;
use super::call::Call;
use super::compare_and_swap::CompareAndSwap;
use super::comparison_operation::ComparisonOperation;
use super::deconstruct_record::DeconstructRecord;
use super::deconstruct_union::DeconstructUnion;
use super::if_::If;
use super::load::Load;
use super::pointer_address::PointerAddress;
use super::record_address::RecordAddress;
use super::store::Store;
use super::union_address::UnionAddress;
use crate::types::{self, Type};

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    AllocateHeap(AllocateHeap),
    AllocateStack(AllocateStack),
    ArithmeticOperation(ArithmeticOperation),
    AtomicLoad(AtomicLoad),
    AtomicStore(AtomicStore),
    Call(Call),
    CompareAndSwap(CompareAndSwap),
    ComparisonOperation(ComparisonOperation),
    DeconstructRecord(DeconstructRecord),
    DeconstructUnion(DeconstructUnion),
    If(If),
    Load(Load),
    PointerAddress(PointerAddress),
    RecordAddress(RecordAddress),
    Store(Store),
    UnionAddress(UnionAddress),
}

impl Instruction {
    pub fn name(&self) -> Option<&str> {
        match self {
            Self::AllocateHeap(allocate) => Some(allocate.name()),
            Self::AllocateStack(allocate) => Some(allocate.name()),
            Self::ArithmeticOperation(operation) => Some(operation.name()),
            Self::AtomicLoad(load) => Some(load.name()),
            Self::Call(call) => Some(call.name()),
            Self::CompareAndSwap(cas) => Some(cas.name()),
            Self::ComparisonOperation(operation) => Some(operation.name()),
            Self::DeconstructRecord(deconstruct) => Some(deconstruct.name()),
            Self::DeconstructUnion(deconstruct) => Some(deconstruct.name()),
            Self::If(if_) => Some(if_.name()),
            Self::Load(load) => Some(load.name()),
            Self::PointerAddress(address) => Some(address.name()),
            Self::RecordAddress(address) => Some(address.name()),
            Self::UnionAddress(address) => Some(address.name()),
            Self::AtomicStore(_) | Self::Store(_) => None,
        }
    }

    pub fn result_type(&self) -> Option<Type> {
        match self {
            Self::AllocateHeap(allocate) => {
                Some(types::Pointer::new(allocate.type_().clone()).into())
            }
            Self::AllocateStack(allocate) => {
                Some(types::Pointer::new(allocate.type_().clone()).into())
            }
            Self::ArithmeticOperation(operation) => Some(operation.type_().clone().into()),
            Self::AtomicLoad(load) => Some(load.type_().clone()),
            Self::Call(call) => Some(call.type_().result().clone()),
            Self::CompareAndSwap(cas) => Some(types::Primitive::Boolean.into()),
            Self::ComparisonOperation(operation) => Some(types::Primitive::Boolean.into()),
            Self::DeconstructRecord(deconstruct) => {
                Some(deconstruct.type_().elements()[deconstruct.element_index()].clone())
            }
            Self::DeconstructUnion(deconstruct) => {
                Some(deconstruct.type_().members()[deconstruct.member_index()].clone())
            }
            Self::If(if_) => Some(if_.type_().clone()),
            Self::Load(load) => Some(load.type_().clone()),
            Self::PointerAddress(address) => Some(address.type_().clone().into()),
            Self::RecordAddress(address) => Some(
                types::Pointer::new(address.type_().elements()[address.element_index()].clone())
                    .into(),
            ),
            Self::UnionAddress(address) => Some(
                types::Pointer::new(address.type_().members()[address.member_index()].clone())
                    .into(),
            ),
            Self::AtomicStore(_) | Self::Store(_) => None,
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

impl From<ArithmeticOperation> for Instruction {
    fn from(operation: ArithmeticOperation) -> Self {
        Self::ArithmeticOperation(operation)
    }
}

impl From<AtomicLoad> for Instruction {
    fn from(load: AtomicLoad) -> Self {
        Self::AtomicLoad(load)
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

impl From<ComparisonOperation> for Instruction {
    fn from(operation: ComparisonOperation) -> Self {
        Self::ComparisonOperation(operation)
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

impl From<PointerAddress> for Instruction {
    fn from(calculation: PointerAddress) -> Self {
        Self::PointerAddress(calculation)
    }
}

impl From<RecordAddress> for Instruction {
    fn from(address: RecordAddress) -> Self {
        Self::RecordAddress(address)
    }
}

impl From<Store> for Instruction {
    fn from(store: Store) -> Self {
        Self::Store(store)
    }
}

impl From<UnionAddress> for Instruction {
    fn from(address: UnionAddress) -> Self {
        Self::UnionAddress(address)
    }
}
