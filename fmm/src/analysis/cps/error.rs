use crate::{
    analysis::{name::NameError, type_check::TypeCheckError},
    build::BuildError,
    ir::*,
};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Clone, Debug, PartialEq)]
pub enum CpsTransformationError {
    Build(BuildError),
    InvalidCallingConvention(Call),
    Name(NameError),
    TypeCheck(TypeCheckError),
}

impl Display for CpsTransformationError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{:?}", self)
    }
}

impl Error for CpsTransformationError {}

impl From<BuildError> for CpsTransformationError {
    fn from(error: BuildError) -> Self {
        Self::Build(error)
    }
}

impl From<NameError> for CpsTransformationError {
    fn from(error: NameError) -> Self {
        Self::Name(error)
    }
}

impl From<TypeCheckError> for CpsTransformationError {
    fn from(error: TypeCheckError) -> Self {
        Self::TypeCheck(error)
    }
}
