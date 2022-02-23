use crate::CompileError;

pub fn compile_calling_convention(
    calling_convention: fmm::types::CallingConvention,
) -> Result<u32, CompileError> {
    match calling_convention {
        fmm::types::CallingConvention::Source | fmm::types::CallingConvention::Target => {
            Ok(llvm_sys::LLVMCallConv::LLVMCCallConv as u32)
        }
        // TODO Use LLVMTailCallConv when it is added to the LLVM C API.
        fmm::types::CallingConvention::Tail => Ok(llvm_sys::LLVMCallConv::LLVMFastCallConv as u32),
        fmm::types::CallingConvention::Trampoline => Err(
            CompileError::UnsupportedCallingConvention(calling_convention),
        ),
    }
}
