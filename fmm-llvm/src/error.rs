use std::error::Error;

#[derive(Clone, Debug, PartialEq)]
pub enum CompileError {
    ConstantNotFolded,
    Llvm(String),
    TargetMachineNotCreated,
    TypeCheck(fmm::analysis::type_check::TypeCheckError),
    Name(fmm::analysis::name::NameError),
}

impl Error for CompileError {}

impl std::fmt::Display for CompileError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::ConstantNotFolded => {
                write!(formatter, "failed to fold constant")
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

impl From<inkwell::support::LLVMString> for CompileError {
    fn from(string: inkwell::support::LLVMString) -> Self {
        Self::Llvm(string.to_string())
    }
}

impl From<inkwell::builder::BuilderError> for CompileError {
    fn from(error: inkwell::builder::BuilderError) -> Self {
        Self::Llvm(error.to_string())
    }
}

impl From<inkwell::values::InstructionValueError> for CompileError {
    fn from(error: inkwell::values::InstructionValueError) -> Self {
        Self::Llvm(error.to_string())
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
