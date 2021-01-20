use super::allocate_heap::AllocateHeap;
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

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    AllocateHeap(AllocateHeap),
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

impl From<AllocateHeap> for Instruction {
    fn from(allocate_heap: AllocateHeap) -> Self {
        Self::AllocateHeap(allocate_heap)
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
