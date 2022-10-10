mod error;

use self::error::CCallingConventionError;
use super::{type_check, type_size};
use crate::{
    ir::*,
    types::{self, void_type, CallingConvention, Type},
};
use fnv::FnvHashSet;

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
    let mut changed_names = FnvHashSet::default();
    let mut function_declarations = vec![];

    for declaration in module.function_declarations() {
        if let Some(declaration) = transform_function_declaration(&context, declaration) {
            changed_names.insert(declaration.name().to_owned());
            function_declarations.push(declaration);
        } else {
            function_declarations.push(declaration.clone());
        }
    }

    let mut function_definitions = vec![];

    for definition in module.function_definitions() {
        if let Some(definition) = transform_function_definition(&context, definition) {
            changed_names.insert(definition.name().to_owned());
            function_definitions.push(definition);
        } else {
            function_definitions.push(definition.clone());
        }
    }

    let module = Module::new(
        module.variable_declarations().to_vec(),
        function_declarations,
        module.variable_definitions().to_vec(),
        function_definitions
            .iter()
            .map(|definition| transform_calls_in_function_definition(definition, &changed_names))
            .collect(),
    );

    type_check::check(&module)?;

    Ok(module)
}

fn transform_function_declaration(
    context: &Context,
    declaration: &FunctionDeclaration,
) -> Option<FunctionDeclaration> {
    transform_function_type(context, declaration.type_())
        .map(|type_| FunctionDeclaration::new(declaration.name(), type_))
}

fn transform_function_definition(
    context: &Context,
    definition: &FunctionDefinition,
) -> Option<FunctionDefinition> {
    if definition.type_().calling_convention() == CallingConvention::Target
        && is_memory_class(context, definition.result_type())
    {
        // TODO
        Some(definition.clone())
    } else {
        None
    }
}

fn transform_calls_in_function_definition(
    definition: &FunctionDefinition,
    names: &FnvHashSet<String>,
) -> FunctionDefinition {
    FunctionDefinition::new(
        definition.name(),
        definition.arguments().to_vec(),
        definition.result_type().clone(),
        transform_block(definition.body(), names),
        definition.options().clone(),
    )
}

fn transform_block(block: &Block, names: &FnvHashSet<String>) -> Block {
    let mut instructions = vec![];

    for instruction in block.instructions() {
        instructions.extend(transform_instruction(instruction, names));
    }

    Block::new(instructions, block.terminal_instruction().clone())
}

fn transform_instruction(
    instruction: &Instruction,
    names: &FnvHashSet<String>,
) -> Vec<Instruction> {
    match instruction {
        Instruction::Call(call) => match call.function() {
            Expression::Variable(variable) if names.contains(variable.name()) => todo!(),
            _ => vec![call.clone().into()],
        },
        _ => vec![instruction.clone()],
    }
}

fn transform_function_type(
    context: &Context,
    function: &types::Function,
) -> Option<types::Function> {
    if function.calling_convention() == CallingConvention::Target
        && is_memory_class(context, function.result())
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
    use pretty_assertions::assert_eq;

    #[test]
    fn transform_empty() {
        assert_eq!(
            transform(&Module::new(vec![], vec![], vec![], vec![]), 8),
            Ok(Module::new(vec![], vec![], vec![], vec![]))
        );
    }

    #[test]
    fn do_not_transform_function_declaration() {
        let module = Module::new(
            vec![],
            vec![FunctionDeclaration::new(
                "f",
                types::Function::new(
                    vec![],
                    types::Record::new(vec![
                        types::Primitive::Integer64.into(),
                        types::Primitive::Integer64.into(),
                    ]),
                    types::CallingConvention::Target,
                ),
            )],
            vec![],
            vec![],
        );

        assert_eq!(transform(&module, 8), Ok(module));
    }

    #[test]
    fn transform_function_declaration() {
        let result_type = types::Record::new(vec![
            types::Primitive::Integer64.into(),
            types::Primitive::Integer64.into(),
            types::Primitive::Integer64.into(),
        ]);

        assert_eq!(
            transform(
                &Module::new(
                    vec![],
                    vec![FunctionDeclaration::new(
                        "f",
                        types::Function::new(
                            vec![],
                            result_type.clone(),
                            types::CallingConvention::Target,
                        )
                    )],
                    vec![],
                    vec![]
                ),
                8
            ),
            Ok(Module::new(
                vec![],
                vec![FunctionDeclaration::new(
                    "f",
                    types::Function::new(
                        vec![types::Pointer::new(result_type).into()],
                        void_type(),
                        types::CallingConvention::Target,
                    )
                )],
                vec![],
                vec![]
            ))
        );
    }
}
