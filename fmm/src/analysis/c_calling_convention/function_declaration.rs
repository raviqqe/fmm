use super::{context::Context, type_};
use crate::ir::FunctionDeclaration;

pub fn transform(
    context: &Context,
    declaration: &FunctionDeclaration,
) -> Option<FunctionDeclaration> {
    type_::transform_function(context, declaration.type_())
        .map(|type_| FunctionDeclaration::new(declaration.name(), type_))
}
