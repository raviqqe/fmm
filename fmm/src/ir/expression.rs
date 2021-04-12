use super::bit_cast::BitCast;
use super::bitwise_operation::BitwiseOperation;
use super::primitive::Primitive;
use super::record::Record;
use super::size_of::SizeOf;
use super::undefined::Undefined;
use super::union::Union;
use super::variable::Variable;
use super::{align_of::AlignOf, bitwise_not_operation::BitwiseNotOperation};

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    AlignOf(AlignOf),
    BitCast(BitCast),
    BitwiseNotOperation(BitwiseNotOperation),
    BitwiseOperation(BitwiseOperation),
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
