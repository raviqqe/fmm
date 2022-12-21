use inkwell::types::BasicType;

pub fn compile_union_cast<'c>(
    builder: &inkwell::builder::Builder<'c>,
    union: inkwell::values::BasicValueEnum<'c>,
    to: inkwell::types::BasicTypeEnum<'c>,
) -> inkwell::values::BasicValueEnum<'c> {
    let pointer = builder.build_alloca(union.get_type(), "");

    builder.build_store(pointer, union);

    builder.build_load(
        builder
            .build_bitcast(pointer, to.ptr_type(Default::default()), "")
            .into_pointer_value(),
        "",
    )
}
