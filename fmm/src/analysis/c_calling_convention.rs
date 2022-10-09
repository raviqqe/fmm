mod error;

use self::error::CCallingConventionError;
use super::{type_check, type_size};
use crate::{
    ir::*,
    types::{self, void_type, CallingConvention, Type},
};

struct Context {
    word_bytes: usize,
}

// TODO Implement the complete C calling convention for all targets.
//
// Based on: https://refspecs.linuxfoundation.org/elf/x86_64-SysV-psABI.pdf
pub fn transform(module: &Module, word_bytes: usize) -> Result<Module, CCallingConventionError> {
    if ![4, 8].contains(&word_bytes) {
        return Err(CCallingConventionError::WordSize(word_bytes));
    }

    type_check::check(module)?;

    let context = Context { word_bytes };
    let module = Module::new(
        module.variable_declarations().to_vec(),
        module
            .function_declarations()
            .iter()
            .map(|declaration| transform_function_declaration(&context, declaration))
            .collect(),
        module.variable_definitions().to_vec(),
        module
            .function_definitions()
            .iter()
            .map(|definition| transform_function_definition(&context, definition))
            .collect(),
    );

    type_check::check(&module)?;

    Ok(module)
}

fn transform_function_declaration(
    context: &Context,
    declaration: &FunctionDeclaration,
) -> FunctionDeclaration {
    FunctionDeclaration::new(
        declaration.name(),
        transform_function_type(context, declaration.type_()),
    )
}

fn transform_function_definition(
    _context: &Context,
    _definition: &FunctionDefinition,
) -> FunctionDefinition {
    todo!()
}

fn transform_function_type(context: &Context, function: &types::Function) -> types::Function {
    if function.calling_convention() == CallingConvention::Target
        && is_memory_class(context, function.result())
    {
        types::Function::new(
            function
                .arguments()
                .iter()
                .cloned()
                .chain([types::Pointer::new(function.result().clone()).into()])
                .collect(),
            void_type(),
            function.calling_convention(),
        )
    } else {
        function.clone()
    }
}

// The name, "memory class" comes from the C ABI on System V.
fn is_memory_class(context: &Context, type_: &Type) -> bool {
    match type_ {
        Type::Record(record) => {
            type_size::calculate_size(type_, context.word_bytes) > 2 * context.word_bytes
                || record
                    .fields()
                    .iter()
                    .any(|type_| is_memory_class(context, type_))
        }
        Type::Union(union) => union
            .members()
            .iter()
            .any(|type_| is_memory_class(context, type_)),
        Type::Function(_) | Type::Pointer(_) | Type::Primitive(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn transform_empty() {
        assert_eq!(
            transform(&Module::new(vec![], vec![], vec![], vec![]), 8),
            Ok(Module::new(vec![], vec![], vec![], vec![]))
        );
    }
}
