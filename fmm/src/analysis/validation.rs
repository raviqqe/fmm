use super::{
    name::{self, NameError},
    type_check::{self, TypeCheckError},
};
use crate::ir::Module;

pub fn validate(module: &Module) -> Result<(), ValidationError> {
    name::check(module)?;
    type_check::check(module)?;

    Ok(())
}

pub enum ValidationError {
    Name(NameError),
    TypeCheck(TypeCheckError),
}

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
