use crate::analysis::type_check::TypeCheckError;
use crate::{build::BuildError, ir::*};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Clone, Debug, PartialEq)]
pub enum CpsTransformationError {
    Build(BuildError),
    InvalidCallingConvention(Call),
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

impl From<TypeCheckError> for CpsTransformationError {
    fn from(error: TypeCheckError) -> Self {
        Self::TypeCheck(error)
    }
}
