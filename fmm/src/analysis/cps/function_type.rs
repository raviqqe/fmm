use super::stack::stack_type;
use crate::{
    analysis::{cps::continuation_type, type_conversion},
    ir::*,
    types::{self, CallingConvention, Type},
};

pub fn transform(module: &Module, continuation_result_type: &Type) -> Module {
    type_conversion::convert(module, &|type_| match type_ {
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
                continuation_type::compile(type_.result(), continuation_result_type).into(),
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
