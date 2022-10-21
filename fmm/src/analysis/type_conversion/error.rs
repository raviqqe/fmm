use crate::types::Type;
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeConversionError {
    FunctionExpected(Type),
    PrimitiveExpected(Type),
    RecordExpected(Type),
    UnionExpected(Type),
}

impl Display for TypeConversionError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{:?}", self)
    }
}

impl Error for TypeConversionError {}
