use super::stack::STACK_TYPE;
use crate::types::{self, CallingConvention, Type};

pub fn create_continuation_type(
    result_type: &Type,
    continuation_result_type: &Type,
) -> types::Function {
    types::Function::new(
        vec![STACK_TYPE.clone(), result_type.clone()],
        continuation_result_type.clone(),
        CallingConvention::Tail,
    )
}
