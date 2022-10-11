use super::{context::Context, function_type};
use crate::ir::FunctionDeclaration;

pub fn transform(
    context: &Context,
    declaration: &FunctionDeclaration,
) -> Option<FunctionDeclaration> {
    function_type::transform(context, declaration.type_())
        .map(|type_| FunctionDeclaration::new(declaration.name(), type_))
}
