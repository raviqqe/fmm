pub fn compile_calling_convention(calling_convention: fmm::types::CallingConvention) -> u32 {
    match calling_convention {
        fmm::types::CallingConvention::Source => 0,
        fmm::types::CallingConvention::Tail => 8,
        fmm::types::CallingConvention::Target => 0,
    }
}
