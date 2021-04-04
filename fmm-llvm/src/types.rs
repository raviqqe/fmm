use fmm::types::{self, Type};
use inkwell::types::BasicType;

pub const DEFAULT_ADDRESS_SPACE: inkwell::AddressSpace = inkwell::AddressSpace::Generic;

pub fn compile_type<'c>(
    type_: &Type,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::BasicTypeEnum<'c> {
    match type_ {
        Type::Function(function) => {
            compile_function_pointer_type(function, context, target_data).into()
        }
        Type::Primitive(primitive) => compile_primitive_type(*primitive, context, target_data),
        Type::Record(record) => compile_record_type(record, context, target_data).into(),
        Type::Pointer(pointer) => compile_pointer_type(pointer, context, target_data).into(),
        Type::TaggedUnion(union) => compile_tagged_union_type(union, context, target_data).into(),
        Type::Union(union) => compile_union_type(union, context, target_data).into(),
    }
}

pub fn compile_function_type<'c>(
    function: &types::Function,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::FunctionType<'c> {
    let compile_type = |type_| compile_type(type_, context, target_data);

    compile_type(function.result()).fn_type(
        &function
            .arguments()
            .iter()
            .map(|type_| compile_type(type_))
            .collect::<Vec<_>>(),
        false,
    )
}

pub fn compile_function_pointer_type<'c>(
    function: &types::Function,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::PointerType<'c> {
    compile_function_type(function, context, target_data).ptr_type(DEFAULT_ADDRESS_SPACE)
}

pub fn compile_pointer_type<'c>(
    pointer: &types::Pointer,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::PointerType<'c> {
    compile_type(pointer.element(), context, target_data).ptr_type(DEFAULT_ADDRESS_SPACE)
}

pub fn compile_primitive_type<'c>(
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
        types::Primitive::PointerInteger => {
            compile_pointer_integer_type(context, target_data).into()
        }
    }
}

pub fn compile_pointer_integer_type<'c>(
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::IntType<'c> {
    context.ptr_sized_int_type(target_data, None)
}

pub fn compile_record_type<'c>(
    record: &types::Record,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::StructType<'c> {
    let compile_type = |type_| compile_type(type_, context, target_data);

    context.struct_type(
        &record
            .elements()
            .iter()
            .map(|type_| compile_type(type_))
            .collect::<Vec<_>>(),
        false,
    )
}

pub fn compile_tagged_union_type<'c>(
    union: &types::TaggedUnion,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::StructType<'c> {
    let integer_type = context.ptr_sized_int_type(target_data, None);

    context.struct_type(
        &[
            compile_primitive_type(union.tag(), context, target_data),
            integer_type
                .array_type(get_pointer_integer_array_size(
                    get_union_size(
                        union.members().iter().map(|member| member.payload()),
                        context,
                        target_data,
                    ) as usize,
                    target_data.get_store_size(&integer_type) as usize,
                ) as u32)
                .into(),
        ],
        false,
    )
}

pub fn compile_union_type<'c>(
    union: &types::Union,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::StructType<'c> {
    let integer_type = context.ptr_sized_int_type(target_data, None);

    context.struct_type(
        &[integer_type
            .array_type(get_pointer_integer_array_size(
                get_union_size(union.members(), context, target_data) as usize,
                target_data.get_store_size(&integer_type) as usize,
            ) as u32)
            .into()],
        false,
    )
}

pub fn compile_union_member_type<'c>(
    union: &types::Union,
    member_index: usize,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::StructType<'c> {
    context.struct_type(
        &[
            compile_type(&union.members()[member_index], context, target_data),
            compile_union_member_padding_type(union, member_index, context, target_data).into(),
        ],
        false,
    )
}

pub fn compile_union_member_padding_type<'c>(
    union: &types::Union,
    member_index: usize,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::ArrayType<'c> {
    let member_type = compile_type(&union.members()[member_index], context, target_data);

    context.i8_type().array_type(
        (get_union_size(union.members(), context, target_data)
            - target_data.get_store_size(&member_type)) as u32,
    )
}

fn get_union_size<'c, 'a>(
    members: impl IntoIterator<Item = &'a Type>,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> u64 {
    members
        .into_iter()
        .map(|type_| target_data.get_store_size(&compile_type(type_, context, target_data)))
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
