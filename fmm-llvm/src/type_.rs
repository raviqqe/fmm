use crate::context::Context;
use fmm::types::{self, Type};
use inkwell::types::BasicType;

pub const DEFAULT_ADDRESS_SPACE: inkwell::AddressSpace = inkwell::AddressSpace::Generic;

pub fn compile<'c>(context: &Context<'c>, type_: &Type) -> inkwell::types::BasicTypeEnum<'c> {
    match type_ {
        Type::Function(function) => compile_function_pointer(context, function).into(),
        Type::Primitive(primitive) => compile_primitive(context, *primitive),
        Type::Record(record) => compile_record(context, record).into(),
        Type::Pointer(pointer) => compile_pointer(context, pointer).into(),
        Type::Union(union) => compile_union(context, union).into(),
    }
}

pub fn compile_function<'c>(
    context: &Context<'c>,
    function: &types::Function,
) -> inkwell::types::FunctionType<'c> {
    let compile_type = |type_| compile(context, type_);

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
    context: &Context<'c>,
    function: &types::Function,
) -> inkwell::types::PointerType<'c> {
    compile_function(context, function).ptr_type(DEFAULT_ADDRESS_SPACE)
}

pub fn compile_pointer<'c>(
    context: &Context<'c>,
    pointer: &types::Pointer,
) -> inkwell::types::PointerType<'c> {
    compile(context, pointer.element()).ptr_type(DEFAULT_ADDRESS_SPACE)
}

pub fn compile_primitive<'c>(
    context: &Context<'c>,
    primitive: types::Primitive,
) -> inkwell::types::BasicTypeEnum<'c> {
    match primitive {
        types::Primitive::Boolean => context.inkwell().bool_type().into(),
        types::Primitive::Float32 => context.inkwell().f32_type().into(),
        types::Primitive::Float64 => context.inkwell().f64_type().into(),
        types::Primitive::Integer8 => context.inkwell().i8_type().into(),
        types::Primitive::Integer32 => context.inkwell().i32_type().into(),
        types::Primitive::Integer64 => context.inkwell().i64_type().into(),
        types::Primitive::PointerInteger => compile_pointer_integer(context).into(),
    }
}

pub fn compile_pointer_integer<'c>(context: &Context<'c>) -> inkwell::types::IntType<'c> {
    context
        .inkwell()
        .ptr_sized_int_type(&context.target_data(), None)
}

pub fn compile_record<'c>(
    context: &Context<'c>,
    record: &types::Record,
) -> inkwell::types::StructType<'c> {
    let compile_type = |type_| compile(context, type_);

    context.inkwell().struct_type(
        &record.fields().iter().map(compile_type).collect::<Vec<_>>(),
        false,
    )
}

pub fn compile_union<'c>(
    context: &Context<'c>,
    union: &types::Union,
) -> inkwell::types::StructType<'c> {
    let target_data = context.target_data();
    let integer_type = context.inkwell().ptr_sized_int_type(&target_data, None);

    context.inkwell().struct_type(
        &[integer_type
            .array_type(get_pointer_integer_array_size(
                get_union_size(context, union) as usize,
                target_data.get_store_size(&integer_type) as usize,
            ) as u32)
            .into()],
        false,
    )
}

pub fn compile_union_member<'c>(
    context: &Context<'c>,
    union: &types::Union,
    member_index: usize,
) -> inkwell::types::StructType<'c> {
    context.inkwell().struct_type(
        &[
            compile(context, &union.members()[member_index]),
            compile_union_member_padding(context, union, member_index).into(),
        ],
        false,
    )
}

pub fn compile_union_member_padding<'c>(
    context: &Context<'c>,
    union: &types::Union,
    member_index: usize,
) -> inkwell::types::ArrayType<'c> {
    let member_type = compile(context, &union.members()[member_index]);

    context.inkwell().i8_type().array_type(
        (get_union_size(context, union) - context.target_data().get_store_size(&member_type))
            as u32,
    )
}

fn get_union_size(context: &Context, union: &types::Union) -> u64 {
    let target_data = context.target_data();

    union
        .members()
        .iter()
        .map(|type_| target_data.get_store_size(&compile(context, type_)))
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
