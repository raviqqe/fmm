use crate::{build::BuildError, ir::*};

#[derive(Clone, Debug, PartialEq)]
pub enum CpsTransformationError {
    BuildError(BuildError),
    InvalidCallingConvention(Call),
}

impl From<BuildError> for CpsTransformationError {
    fn from(error: BuildError) -> Self {
        Self::BuildError(error)
    }
}
