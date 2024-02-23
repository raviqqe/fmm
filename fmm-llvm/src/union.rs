use inkwell::types::BasicType;

use crate::CompileError;

pub fn compile_union_cast<'c>(
    builder: &inkwell::builder::Builder<'c>,
    union: inkwell::values::BasicValueEnum<'c>,
    to: inkwell::types::BasicTypeEnum<'c>,
) -> Result<inkwell::values::BasicValueEnum<'c>, CompileError> {
    let pointer = builder.build_alloca(union.get_type(), "")?;

    builder.build_store(pointer, union)?;

    Ok(builder.build_load(
        to,
        builder
            .build_bitcast(pointer, to.ptr_type(Default::default()), "")?
            .into_pointer_value(),
        "",
    )?)
}
