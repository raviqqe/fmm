use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Clone, Debug)]
pub enum ReferenceCountError {
    UnionNotSupported,
}

impl Display for ReferenceCountError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{:?}", self)
    }
}

impl Error for ReferenceCountError {}
