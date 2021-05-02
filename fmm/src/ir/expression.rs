use super::{
    align_of::AlignOf, arithmetic_operation::ArithmeticOperation, bit_cast::BitCast,
    bitwise_not_operation::BitwiseNotOperation, bitwise_operation::BitwiseOperation,
    comparison_operation::ComparisonOperation, primitive::Primitive, record::Record,
    size_of::SizeOf, undefined::Undefined, union::Union, variable::Variable,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    AlignOf(AlignOf),
    ArithmeticOperation(ArithmeticOperation),
    BitCast(BitCast),
    BitwiseNotOperation(BitwiseNotOperation),
    BitwiseOperation(BitwiseOperation),
    ComparisonOperation(ComparisonOperation),
    Primitive(Primitive),
    Record(Record),
    SizeOf(SizeOf),
    Undefined(Undefined),
    Union(Union),
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

impl From<Variable> for Expression {
    fn from(variable: Variable) -> Self {
        Self::Variable(variable)
    }
}
