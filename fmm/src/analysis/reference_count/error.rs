use crate::build::BuildError;
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Clone, Debug)]
pub enum ReferenceCountError {
    BuildError(BuildError),
    UnionNotSupported,
}

impl Display for ReferenceCountError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{:?}", self)
    }
}

impl Error for ReferenceCountError {}

impl From<BuildError> for ReferenceCountError {
    fn from(error: BuildError) -> Self {
        Self::BuildError(error)
    }
}
