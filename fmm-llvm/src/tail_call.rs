use llvm_sys::ir_reader::*;
use llvm_sys::{bit_writer::LLVMWriteBitcodeToMemoryBuffer, core::*};

pub fn convert_tail_to_must_tail<'c>(
    context: &'c inkwell::context::Context,
    module: &inkwell::module::Module<'c>,
) -> Result<inkwell::module::Module<'c>, inkwell::support::LLVMString> {
    let ir = module
        .print_to_string()
        .to_string()
        .replace(" tail ", " musttail ");

    let buffer = unsafe {
        let context = LLVMContextCreate();
        let mut module = LLVMModuleCreateWithName(std::ptr::null());
        let ir_buffer = LLVMCreateMemoryBufferWithMemoryRangeCopy(
            ir.as_ptr() as *const i8,
            ir.len(),
            std::ptr::null(),
        );

        let error = LLVMParseIRInContext(context, ir_buffer, &mut module, std::ptr::null_mut());

        if error > 0 {
            unreachable!("invalid LLVM IR generated")
        }

        let bitcode_buffer = LLVMWriteBitcodeToMemoryBuffer(module);
        let inkwell_buffer = inkwell::memory_buffer::MemoryBuffer::create_from_memory_range_copy(
            std::slice::from_raw_parts(
                LLVMGetBufferStart(bitcode_buffer) as *const u8,
                LLVMGetBufferSize(bitcode_buffer),
            ),
            "",
        );

        // ir_buffer seems to be consumed by LLVMParseIRInContext?
        LLVMDisposeMemoryBuffer(bitcode_buffer);
        LLVMDisposeModule(module);
        LLVMContextDispose(context);

        inkwell_buffer
    };

    inkwell::module::Module::parse_bitcode_from_buffer(&buffer, &context)
}
