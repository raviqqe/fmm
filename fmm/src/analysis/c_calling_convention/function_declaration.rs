use super::{context::Context, utilities};
use crate::{
    ir::FunctionDeclaration,
    types::{self, void_type},
};

pub fn transform(
    context: &Context,
    declaration: &FunctionDeclaration,
) -> Option<FunctionDeclaration> {
    transform_function_type(context, declaration.type_())
        .map(|type_| FunctionDeclaration::new(declaration.name(), type_))
}

fn transform_function_type(
    context: &Context,
    function: &types::Function,
) -> Option<types::Function> {
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
