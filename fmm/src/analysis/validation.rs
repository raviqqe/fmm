use super::{
    name::{self, NameError},
    type_check::{self, TypeCheckError},
};
use crate::ir::Module;
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

pub fn validate(module: &Module) -> Result<(), ValidationError> {
    name::check(module)?;
    type_check::check(module)?;

    Ok(())
}

#[derive(Clone, Debug, PartialEq)]
pub enum ValidationError {
    Name(NameError),
    TypeCheck(TypeCheckError),
}

impl Display for ValidationError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{:?}", self)
    }
}

impl Error for ValidationError {}

impl From<NameError> for ValidationError {
    fn from(error: NameError) -> Self {
        Self::Name(error)
    }
}

impl From<TypeCheckError> for ValidationError {
    fn from(error: TypeCheckError) -> Self {
        Self::TypeCheck(error)
    }
}
