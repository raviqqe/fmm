use super::{context::Context, utilities};
use crate::types::{self, void_type};

pub fn transform(context: &Context, function: &types::Function) -> Option<types::Function> {
    if function.calling_convention() == types::CallingConvention::Target
        && utilities::is_memory_class(context, function.result())
    {
        Some(types::Function::new(
            function
                .arguments()
                .iter()
                .cloned()
                .chain([types::Pointer::new(function.result().clone()).into()])
                .collect(),
            void_type(),
            function.calling_convention(),
        ))
    } else {
        None
    }
}
