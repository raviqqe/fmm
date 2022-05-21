use fmm::types::{self, Type};
use inkwell::types::BasicType;

pub const DEFAULT_ADDRESS_SPACE: inkwell::AddressSpace = inkwell::AddressSpace::Generic;

pub fn compile<'c>(
    type_: &Type,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::BasicTypeEnum<'c> {
    match type_ {
        Type::Function(function) => compile_function_pointer(function, context, target_data).into(),
        Type::Primitive(primitive) => compile_primitive(*primitive, context, target_data),
        Type::Record(record) => compile_record(record, context, target_data).into(),
        Type::Pointer(pointer) => compile_pointer(pointer, context, target_data).into(),
        Type::Union(union) => compile_union(union, context, target_data).into(),
    }
}

pub fn compile_function<'c>(
    function: &types::Function,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::FunctionType<'c> {
    let compile_type = |type_| compile(type_, context, target_data);

    compile_type(function.result()).fn_type(
        &function
            .arguments()
            .iter()
            .map(|type_| compile_type(type_).into())
            .collect::<Vec<_>>(),
        false,
    )
}

pub fn compile_function_pointer<'c>(
    function: &types::Function,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::PointerType<'c> {
    compile_function(function, context, target_data).ptr_type(DEFAULT_ADDRESS_SPACE)
}

pub fn compile_pointer<'c>(
    pointer: &types::Pointer,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::PointerType<'c> {
    compile(pointer.element(), context, target_data).ptr_type(DEFAULT_ADDRESS_SPACE)
}

pub fn compile_primitive<'c>(
    primitive: types::Primitive,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::BasicTypeEnum<'c> {
    match primitive {
        types::Primitive::Boolean => context.bool_type().into(),
        types::Primitive::Float32 => context.f32_type().into(),
        types::Primitive::Float64 => context.f64_type().into(),
        types::Primitive::Integer8 => context.i8_type().into(),
        types::Primitive::Integer32 => context.i32_type().into(),
        types::Primitive::Integer64 => context.i64_type().into(),
        types::Primitive::PointerInteger => compile_pointer_integer(context, target_data).into(),
    }
}

pub fn compile_pointer_integer<'c>(
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::IntType<'c> {
    context.ptr_sized_int_type(target_data, None)
}

pub fn compile_record<'c>(
    record: &types::Record,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::StructType<'c> {
    let compile_type = |type_| compile(type_, context, target_data);

    context.struct_type(
        &record.fields().iter().map(compile_type).collect::<Vec<_>>(),
        false,
    )
}

pub fn compile_union<'c>(
    union: &types::Union,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::StructType<'c> {
    let integer_type = context.ptr_sized_int_type(target_data, None);

    context.struct_type(
        &[integer_type
            .array_type(get_pointer_integer_array_size(
                get_union_size(union, context, target_data) as usize,
                target_data.get_store_size(&integer_type) as usize,
            ) as u32)
            .into()],
        false,
    )
}

pub fn compile_union_member<'c>(
    union: &types::Union,
    member_index: usize,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::StructType<'c> {
    context.struct_type(
        &[
            compile(&union.members()[member_index], context, target_data),
            compile_union_member_padding(union, member_index, context, target_data).into(),
        ],
        false,
    )
}

pub fn compile_union_member_padding<'c>(
    union: &types::Union,
    member_index: usize,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::ArrayType<'c> {
    let member_type = compile(&union.members()[member_index], context, target_data);

    context.i8_type().array_type(
        (get_union_size(union, context, target_data) - target_data.get_store_size(&member_type))
            as u32,
    )
}

fn get_union_size<'c>(
    union: &types::Union,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> u64 {
    union
        .members()
        .iter()
        .map(|type_| target_data.get_store_size(&compile(type_, context, target_data)))
        .max()
        .unwrap()
}

fn get_pointer_integer_array_size(size: usize, pointer_size: usize) -> usize {
    if size == 0 {
        0
    } else {
        ((size - 1) / pointer_size + 1) as usize
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_pointer_integer_array_size_for_32_bit_platform() {
        assert_eq!(get_pointer_integer_array_size(0, 4), 0);
        assert_eq!(get_pointer_integer_array_size(1, 4), 1);
        assert_eq!(get_pointer_integer_array_size(3, 4), 1);
        assert_eq!(get_pointer_integer_array_size(4, 4), 1);
        assert_eq!(get_pointer_integer_array_size(5, 4), 2);
    }

    #[test]
    fn get_pointer_integer_array_size_for_64_bit_platform() {
        assert_eq!(get_pointer_integer_array_size(0, 8), 0);
        assert_eq!(get_pointer_integer_array_size(1, 8), 1);
        assert_eq!(get_pointer_integer_array_size(7, 8), 1);
        assert_eq!(get_pointer_integer_array_size(8, 8), 1);
        assert_eq!(get_pointer_integer_array_size(9, 8), 2);
    }
}
