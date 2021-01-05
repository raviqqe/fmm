use crate::ir::*;
use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeCheckError {
    FunctionArguments(Call),
    IndexOutOfRange(Instruction),
    RecordElements(Record),
    TypesNotMatched(Type, Type),
    VariableNotFound(Variable),
}
