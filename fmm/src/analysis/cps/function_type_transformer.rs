use super::stack::stack_type;
use crate::{
    analysis::{convert_types, cps::continuation_type_compiler},
    ir::*,
    types::{self, CallingConvention, Type},
};

pub fn transform(module: &Module, continuation_result_type: &Type) -> Module {
    convert_types(module, &|type_| match type_ {
        Type::Function(function) => {
            transform_function_type(function, continuation_result_type).into()
        }
        _ => type_.clone(),
    })
}

fn transform_function_type(
    type_: &types::Function,
    continuation_result_type: &Type,
) -> types::Function {
    if type_.calling_convention() == CallingConvention::Source {
        types::Function::new(
            [
                stack_type(),
                continuation_type_compiler::compile(type_.result(), continuation_result_type)
                    .into(),
            ]
            .into_iter()
            .chain(type_.arguments().iter().cloned())
            .collect(),
            continuation_result_type.clone(),
            CallingConvention::Tail,
        )
    } else {
        type_.clone()
    }
}
