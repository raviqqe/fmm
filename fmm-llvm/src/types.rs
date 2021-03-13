use fmm::types::{self, Type};
use inkwell::types::BasicType;

const DEFAULT_ADDRESS_SPACE: inkwell::AddressSpace = inkwell::AddressSpace::Generic;

pub fn compile_type<'c>(
    type_: &Type,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::BasicTypeEnum<'c> {
    let compile_type = |type_| compile_type(type_, context, target_data);

    match type_ {
        Type::Function(function) => {
            compile_function_pointer_type(function, context, target_data).into()
        }
        Type::Primitive(primitive) => compile_primitive_type(*primitive, context).into(),
        Type::Record(record) => compile_record_type(record, context, target_data).into(),
        Type::Pointer(pointer) => compile_pointer_type(pointer, context, target_data).into(),
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
) -> inkwell::types::BasicTypeEnum<'c> {
    match primitive {
        types::Primitive::Boolean => context.custom_width_int_type(1).into(),
        types::Primitive::Float32 => context.f32_type().into(),
        types::Primitive::Float64 => context.f64_type().into(),
        types::Primitive::Integer8 => context.i8_type().into(),
        types::Primitive::Integer32 => context.i32_type().into(),
        types::Primitive::Integer64 => context.i64_type().into(),
        types::Primitive::PointerInteger => {
            context.i8_type().ptr_type(DEFAULT_ADDRESS_SPACE).into()
        }
    }
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

pub fn compile_union_type<'c>(
    union: &types::Union,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::types::StructType<'c> {
    let compile_type = |type_| compile_type(type_, context, target_data);

    let size = union
        .members()
        .iter()
        .map(|type_| target_data.get_store_size(&compile_type(type_)))
        .max()
        .unwrap();

    context.struct_type(&[context.i8_type().array_type(size as u32).into()], false)
}
