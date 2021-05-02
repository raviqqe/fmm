use crate::{ir::*, types::Type};

#[derive(Clone, Debug, PartialEq)]
pub enum TypeCheckError {
    FunctionArguments(Call),
    IndexOutOfRange,
    InvalidBranch(Branch),
    RecordElements(Record),
    TypesNotMatched(Type, Type),
    VariableNotFound(Variable),
}
