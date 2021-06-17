use crate::{ir::*, types::Type};
use std::error::Error;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, PartialEq)]
pub enum TypeCheckError {
    DuplicateNames(String),
    FunctionArguments(Call),
    IndexOutOfRange,
    InvalidBranch(Branch),
    RecordElements(Record),
    TypesNotMatched(Type, Type),
    VariableNotFound(Variable),
}

impl Display for TypeCheckError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{:?}", self)
    }
}

impl Error for TypeCheckError {}
