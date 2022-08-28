use std::error::Error;

#[derive(Clone, Debug, PartialEq)]
pub enum CompileError {
    TypeCheck(fmm::analysis::type_check::TypeCheckError),
}

impl Error for CompileError {}

impl std::fmt::Display for CompileError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::TypeCheck(error) => write!(formatter, "{}", error),
        }
    }
}

impl From<fmm::analysis::type_check::TypeCheckError> for CompileError {
    fn from(error: fmm::analysis::type_check::TypeCheckError) -> Self {
        Self::TypeCheck(error)
    }
}
