use crate::{
    analysis::{name::NameError, type_conversion::TypeConversionError},
    build::BuildError,
    ir::*,
};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Clone, Debug, PartialEq)]
pub enum CpsError {
    Build(BuildError),
    InvalidCallingConvention(Call),
    Name(NameError),
    TypeConversion(TypeConversionError),
}

impl Display for CpsError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{:?}", self)
    }
}

impl Error for CpsError {}

impl From<BuildError> for CpsError {
    fn from(error: BuildError) -> Self {
        Self::Build(error)
    }
}

impl From<NameError> for CpsError {
    fn from(error: NameError) -> Self {
        Self::Name(error)
    }
}

impl From<TypeConversionError> for CpsError {
    fn from(error: TypeConversionError) -> Self {
        Self::TypeConversion(error)
    }
}
