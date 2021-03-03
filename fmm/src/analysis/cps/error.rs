use crate::ir::*;

#[derive(Clone, Debug, PartialEq)]
pub enum CpsTransformationError {
    InvalidCallingConvention(Call),
}
