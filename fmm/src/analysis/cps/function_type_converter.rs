use super::stack::STACK_TYPE;
use crate::{
    analysis::{convert_types, cps::utilities::create_continuation_type},
    ir::*,
    types::{self, CallingConvention, Type},
};

pub fn convert(module: &Module, continuation_result_type: &Type) -> Module {
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
            vec![
                STACK_TYPE.clone(),
                create_continuation_type(type_.result(), continuation_result_type).into(),
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
