use super::{context::Context, type_};
use crate::ir::FunctionDeclaration;

pub fn transform(context: &Context, declaration: &FunctionDeclaration) -> FunctionDeclaration {
    FunctionDeclaration::new(
        declaration.name(),
        type_::transform_function(context, declaration.type_()),
    )
}
