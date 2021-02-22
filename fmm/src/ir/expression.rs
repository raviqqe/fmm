use super::align_of::AlignOf;
use super::primitive::Primitive;
use super::record::Record;
use super::size_of::SizeOf;
use super::undefined::Undefined;
use super::union::Union;
use super::variable::Variable;

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    AlignOf(AlignOf),
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
