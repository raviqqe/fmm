use super::stack;
use crate::types::{self, CallingConvention, Type};

pub fn compile(result_type: &Type, continuation_result_type: &Type) -> types::Function {
    types::Function::new(
        vec![stack::type_(), result_type.clone()],
        continuation_result_type.clone(),
        CallingConvention::Tail,
    )
}
