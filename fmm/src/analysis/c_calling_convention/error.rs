use crate::{analysis::type_conversion::TypeConversionError, build::BuildError};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CCallingConventionError {
    Build(BuildError),
    TypeConversion(TypeConversionError),
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

impl From<TypeConversionError> for CCallingConventionError {
    fn from(error: TypeConversionError) -> Self {
        Self::TypeConversion(error)
    }
}
