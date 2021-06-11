use std::error::Error;

#[derive(Clone, Debug, PartialEq)]
pub enum CompileError {
    Llvm(String),
    TargetMachineNotCreated,
}

impl Error for CompileError {}

impl std::fmt::Display for CompileError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CompileError::Llvm(string) => {
                write!(formatter, "{}", string)
            }
            CompileError::TargetMachineNotCreated => {
                write!(formatter, "failed to create target machine")
            }
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
