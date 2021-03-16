pub fn compile_calling_convention(calling_convention: fmm::types::CallingConvention) -> u32 {
    match calling_convention {
        fmm::types::CallingConvention::Source | fmm::types::CallingConvention::Target => {
            llvm_sys::LLVMCallConv::LLVMCCallConv as u32
        }
        fmm::types::CallingConvention::Tail => 18, // llvm/include/llvm/IR/CallingConv.h
    }
}
