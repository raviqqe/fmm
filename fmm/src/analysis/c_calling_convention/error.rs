use crate::{analysis::type_check::TypeCheckError, build::BuildError};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Clone, Debug, PartialEq)]
pub enum CCallingConventionError {
    Build(BuildError),
    TypeCheck(TypeCheckError),
    WordSize(usize),
}

impl Display for CCallingConventionError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{:?}", self)
    }
}

impl Error for CCallingConventionError {}

impl From<BuildError> for CCallingConventionError {
    fn from(error: BuildError) -> Self {
        Self::Build(error)
    }
}

impl From<TypeCheckError> for CCallingConventionError {
    fn from(error: TypeCheckError) -> Self {
        Self::TypeCheck(error)
    }
}
