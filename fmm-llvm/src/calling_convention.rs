pub fn compile(calling_convention: fmm::types::CallingConvention) -> u32 {
    (match calling_convention {
        fmm::types::CallingConvention::Source | fmm::types::CallingConvention::Target => {
            llvm_sys::LLVMCallConv::LLVMCCallConv
        }
        // TODO Use llvm_sys::LLVMCallConv::LLVMTailCallConv.
        fmm::types::CallingConvention::Tail => llvm_sys::LLVMCallConv::LLVMFastCallConv,
    }) as u32
}
