use std::error::Error;

#[derive(Debug, PartialEq)]
pub enum CompileError {
    InkwellBuilder(inkwell::builder::BuilderError),
    Llvm(String),
    Name(fmm::analysis::name::NameError),
    TargetMachineNotCreated,
    TypeCheck(fmm::analysis::type_check::TypeCheckError),
}

impl Error for CompileError {}

impl std::fmt::Display for CompileError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::InkwellBuilder(error) => {
                write!(formatter, "{}", error)
            }
            Self::Llvm(string) => {
                write!(formatter, "{}", string)
            }
            Self::TargetMachineNotCreated => {
                write!(formatter, "failed to create target machine")
            }
            Self::TypeCheck(error) => write!(formatter, "{}", error),
            Self::Name(error) => write!(formatter, "{}", error),
        }
    }
}

impl From<inkwell::builder::BuilderError> for CompileError {
    fn from(error: inkwell::builder::BuilderError) -> Self {
        Self::InkwellBuilder(error)
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

impl From<fmm::analysis::type_check::TypeCheckError> for CompileError {
    fn from(error: fmm::analysis::type_check::TypeCheckError) -> Self {
        Self::TypeCheck(error)
    }
}

impl From<fmm::analysis::name::NameError> for CompileError {
    fn from(error: fmm::analysis::name::NameError) -> Self {
        Self::Name(error)
    }
}
