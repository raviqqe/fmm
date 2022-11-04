use crate::{ir::*, types::Type};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Clone, Debug, PartialEq)]
pub enum VariableScopeError {
    VariableNotFound(Variable),
}

impl Display for VariableScopeError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{:?}", self)
    }
}

impl Error for VariableScopeError {}
