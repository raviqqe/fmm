mod error;

use self::error::CCallingConventionError;
use super::type_check;
use crate::{
    ir::*,
    types::{self, void_type, CallingConvention, Type},
};

struct Context {
    word_bytes: usize,
}

// TODO Implement the complete C calling convention for all targets.
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
    context: &Context,
    definition: &FunctionDefinition,
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
            type_size(context, type_) > 2 * context.word_bytes
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

fn type_size(context: &Context, type_: &Type) -> usize {
    match type_ {
        Type::Record(record) => {
            let mut size = 0;

            for field in record.fields() {
                let field_size = type_size(context, field);

                // Use field sizes as alignment.
                size = size.max((size as f64 / field_size as f64).ceil() as usize * field_size);

                size += field_size;
            }

            size
        }
        Type::Primitive(primitive) => match primitive {
            types::Primitive::Boolean => 1,
            types::Primitive::Float32 => 4,
            types::Primitive::Float64 => 8,
            types::Primitive::Integer8 => 1,
            types::Primitive::Integer32 => 4,
            types::Primitive::Integer64 => 8,
            types::Primitive::PointerInteger => context.word_bytes,
        },
        Type::Union(union) => union
            .members()
            .iter()
            .map(|type_| type_size(context, type_))
            .max()
            .unwrap_or_default(),
        Type::Function(_) | Type::Pointer(_) => context.word_bytes,
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
