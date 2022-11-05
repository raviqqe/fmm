use crate::ir::*;
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VariableScopeError {
    VariableNotFound(Variable),
}

impl Display for VariableScopeError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{:?}", self)
    }
}

impl Error for VariableScopeError {}
