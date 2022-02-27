use std::error::Error;

#[derive(Clone, Debug, PartialEq)]
pub enum CompileError {
    Llvm(String),
    TargetMachineNotCreated,
    TypeCheck(fmm::analysis::TypeCheckError),
}

impl Error for CompileError {}

impl std::fmt::Display for CompileError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Llvm(string) => {
                write!(formatter, "{}", string)
            }
            Self::TargetMachineNotCreated => {
                write!(formatter, "failed to create target machine")
            }
            Self::TypeCheck(error) => write!(formatter, "{}", error),
        }
    }
}

impl From<inkwell::support::LLVMString> for CompileError {
    fn from(string: inkwell::support::LLVMString) -> Self {
        Self::Llvm(string.to_string())
    }
}

impl From<&str> for CompileError {
    fn from(string: &str) -> Self {
        Self::Llvm(string.to_string())
    }
}

impl From<fmm::analysis::TypeCheckError> for CompileError {
    fn from(error: fmm::analysis::TypeCheckError) -> Self {
        Self::TypeCheck(error)
    }
}
