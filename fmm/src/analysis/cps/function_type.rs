use super::stack;
use crate::{
    analysis::{
        cps::continuation_type,
        type_conversion::{self, TypeConversionError},
    },
    ir::*,
    types::{self, CallingConvention, Type},
};

pub fn transform(
    module: &mut Module,
    continuation_result_type: &Type,
) -> Result<(), TypeConversionError> {
    type_conversion::convert(module, &|type_| match type_ {
        Type::Function(function) if function.calling_convention() == CallingConvention::Source => {
            types::Function::new(
                [
                    stack::type_(),
                    continuation_type::compile(function.result(), continuation_result_type).into(),
                ]
                .into_iter()
                .chain(function.arguments().iter().cloned())
                .collect(),
                continuation_result_type.clone(),
                CallingConvention::Tail,
            )
            .into()
        }
        _ => type_.clone(),
    })
}
