use crate::types::Type;
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BuildError {
    FunctionExpected(Type),
    PointerExpected(Type),
    PrimitiveExpected(Type),
    RecordExpected(Type),
    UnionExpected(Type),
}

impl Display for BuildError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{:?}", self)
    }
}

impl Error for BuildError {}
