use super::context::Context;
use crate::{
    analysis::type_size,
    types::{self, void_type, Type},
};

pub fn transform_function(context: &Context, function: &types::Function) -> types::Function {
    if function.calling_convention() == types::CallingConvention::Target
        && is_memory_class(context, function.result())
    {
        types::Function::new(
            function
                .arguments()
                .iter()
                .cloned()
                .chain([types::Pointer::new(function.result().clone()).into()])
                .collect(),
            void_type(),
            function.calling_convention(),
        )
    } else {
        function.clone()
    }
}

// The name, "memory class" comes from the C ABI on System V.
pub fn is_memory_class(context: &Context, type_: &Type) -> bool {
    match type_ {
        Type::Record(record) => {
            type_size::calculate_size(type_, context.word_bytes()) > 2 * context.word_bytes()
                || record
                    .fields()
                    .iter()
                    .any(|type_| is_memory_class(context, type_))
        }
        Type::Union(union) => union
            .members()
            .iter()
            .any(|type_| is_memory_class(context, type_)),
        Type::Function(_) | Type::Pointer(_) | Type::Primitive(_) => false,
    }
}
