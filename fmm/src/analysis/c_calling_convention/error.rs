use crate::analysis::type_check::TypeCheckError;
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Clone, Debug, PartialEq)]
pub enum CCallingConventionError {
    TypeCheck(TypeCheckError),
    WordSize(usize),
}

impl Display for CCallingConventionError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{:?}", self)
    }
}

impl Error for CCallingConventionError {}

impl From<TypeCheckError> for CCallingConventionError {
    fn from(error: TypeCheckError) -> Self {
        Self::TypeCheck(error)
    }
}
