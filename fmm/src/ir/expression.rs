use super::{
    align_of::AlignOf, arithmetic_operation::ArithmeticOperation, bit_cast::BitCast,
    bitwise_not_operation::BitwiseNotOperation, bitwise_operation::BitwiseOperation,
    comparison_operation::ComparisonOperation, pointer_address::PointerAddress,
    primitive::Primitive, record::Record, record_address::RecordAddress, size_of::SizeOf,
    undefined::Undefined, union::Union, union_address::UnionAddress, variable::Variable,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    AlignOf(AlignOf),
    ArithmeticOperation(ArithmeticOperation),
    BitCast(BitCast),
    BitwiseNotOperation(BitwiseNotOperation),
    BitwiseOperation(BitwiseOperation),
    ComparisonOperation(ComparisonOperation),
    PointerAddress(PointerAddress),
    Primitive(Primitive),
    Record(Record),
    RecordAddress(RecordAddress),
    SizeOf(SizeOf),
    Undefined(Undefined),
    Union(Union),
    UnionAddress(UnionAddress),
    Variable(Variable),
}

impl From<AlignOf> for Expression {
    fn from(align_of: AlignOf) -> Self {
        Self::AlignOf(align_of)
    }
}

impl From<ArithmeticOperation> for Expression {
    fn from(operation: ArithmeticOperation) -> Self {
        Self::ArithmeticOperation(operation)
    }
}

impl From<BitCast> for Expression {
    fn from(bit_cast: BitCast) -> Self {
        Self::BitCast(bit_cast)
    }
}

impl From<BitwiseNotOperation> for Expression {
    fn from(operation: BitwiseNotOperation) -> Self {
        Self::BitwiseNotOperation(operation)
    }
}

impl From<BitwiseOperation> for Expression {
    fn from(operation: BitwiseOperation) -> Self {
        Self::BitwiseOperation(operation)
    }
}

impl From<ComparisonOperation> for Expression {
    fn from(operation: ComparisonOperation) -> Self {
        Self::ComparisonOperation(operation)
    }
}

impl From<PointerAddress> for Expression {
    fn from(address: PointerAddress) -> Self {
        Self::PointerAddress(address)
    }
}

impl From<Primitive> for Expression {
    fn from(primitive: Primitive) -> Self {
        Self::Primitive(primitive)
    }
}

impl From<Record> for Expression {
    fn from(record: Record) -> Self {
        Self::Record(record)
    }
}

impl From<RecordAddress> for Expression {
    fn from(address: RecordAddress) -> Self {
        Self::RecordAddress(address)
    }
}

impl From<SizeOf> for Expression {
    fn from(size_of: SizeOf) -> Self {
        Self::SizeOf(size_of)
    }
}

impl From<Undefined> for Expression {
    fn from(undefined: Undefined) -> Self {
        Self::Undefined(undefined)
    }
}

impl From<Union> for Expression {
    fn from(union: Union) -> Self {
        Self::Union(union)
    }
}

impl From<UnionAddress> for Expression {
    fn from(address: UnionAddress) -> Self {
        Self::UnionAddress(address)
    }
}

impl From<Variable> for Expression {
    fn from(variable: Variable) -> Self {
        Self::Variable(variable)
    }
}
