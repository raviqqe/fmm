mod error;

use self::error::CCallingConventionError;
use super::{type_check, type_size};
use crate::{
    ir::*,
    types::{self, void_type, CallingConvention, Type},
};
use fnv::FnvHashMap;

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
    let mut changed_names = FnvHashMap::default();
    let mut function_declarations = vec![];

    for declaration in module.function_declarations() {
        if let Some(declaration) = transform_function_declaration(&context, declaration) {
            changed_names.insert(declaration.name().to_owned(), declaration.type_().clone());
            function_declarations.push(declaration);
        } else {
            function_declarations.push(declaration.clone());
        }
    }

    let mut function_definitions = vec![];

    for definition in module.function_definitions() {
        if let Some(definition) = transform_function_definition(&context, definition) {
            changed_names.insert(definition.name().to_owned(), definition.type_().clone());
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
        let pointer_name = format!("{}.p", definition.name());

        Some(FunctionDefinition::new(
            definition.name(),
            definition
                .arguments()
                .iter()
                .cloned()
                .chain([Argument::new(
                    &pointer_name,
                    types::Pointer::new(definition.result_type().clone()),
                )])
                .collect(),
            void_type(),
            transform_block(definition.body(), definition.result_type(), &pointer_name),
            definition.options().clone(),
        ))
    } else {
        None
    }
}

fn transform_block(block: &Block, result_type: &Type, pointer_name: &str) -> Block {
    if let TerminalInstruction::Return(return_) = block.terminal_instruction() {
        Block::new(
            block
                .instructions()
                .iter()
                .cloned()
                .chain([Store::new(
                    result_type.clone(),
                    return_.expression().clone(),
                    Variable::new(pointer_name),
                )
                .into()])
                .collect(),
            Return::new(void_type(), void_value()),
        )
    } else {
        block.clone()
    }
}

fn transform_calls_in_function_definition(
    definition: &FunctionDefinition,
    names: &FnvHashMap<String, types::Function>,
) -> FunctionDefinition {
    FunctionDefinition::new(
        definition.name(),
        definition.arguments().to_vec(),
        definition.result_type().clone(),
        transform_calls_in_block(definition.body(), names),
        definition.options().clone(),
    )
}

fn transform_calls_in_block(block: &Block, names: &FnvHashMap<String, types::Function>) -> Block {
    let mut instructions = vec![];

    for instruction in block.instructions() {
        instructions.extend(transform_instruction(instruction, names));
    }

    Block::new(instructions, block.terminal_instruction().clone())
}

fn transform_instruction(
    instruction: &Instruction,
    names: &FnvHashMap<String, types::Function>,
) -> Vec<Instruction> {
    match instruction {
        Instruction::Call(call) => match call.function() {
            // TODO Support complex expressions.
            Expression::Variable(variable) => {
                if let Some(type_) = names.get(variable.name()) {
                    let function_type = call.type_();
                    let pointer_name = format!("{}.p", call.name());
                    let pointer = Variable::new(&pointer_name);

                    vec![
                        AllocateStack::new(function_type.result().clone(), &pointer_name).into(),
                        Call::new(
                            type_.clone(),
                            variable.clone(),
                            call.arguments()
                                .iter()
                                .cloned()
                                .chain([pointer.clone().into()])
                                .collect(),
                            format!("{}.r", call.name()),
                        )
                        .into(),
                        Load::new(function_type.result().clone(), pointer, call.name()).into(),
                    ]
                } else {
                    vec![call.clone().into()]
                }
            }
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

    const WORD_BYTES: usize = 8;

    fn transform_module(module: &Module) -> Result<Module, CCallingConventionError> {
        transform(module, WORD_BYTES)
    }

    #[test]
    fn transform_empty() {
        assert_eq!(
            transform_module(&Module::new(vec![], vec![], vec![], vec![])),
            Ok(Module::new(vec![], vec![], vec![], vec![]))
        );
    }

    mod function_declaration {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn do_not_transform_compatible_function_declaration() {
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

            assert_eq!(transform_module(&module), Ok(module));
        }

        #[test]
        fn transform_function_declaration() {
            let result_type = types::Record::new(vec![
                types::Primitive::Integer64.into(),
                types::Primitive::Integer64.into(),
                types::Primitive::Integer64.into(),
            ]);

            assert_eq!(
                transform_module(&Module::new(
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
                )),
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

        #[test]
        fn transform_function_declaration_with_call() {
            let record_type = types::Record::new(vec![
                types::Primitive::Integer64.into(),
                types::Primitive::Integer64.into(),
                types::Primitive::Integer64.into(),
            ]);

            assert_eq!(
                transform_module(&Module::new(
                    vec![],
                    vec![FunctionDeclaration::new(
                        "f",
                        types::Function::new(
                            vec![],
                            record_type.clone(),
                            types::CallingConvention::Target,
                        )
                    )],
                    vec![],
                    vec![FunctionDefinition::new(
                        "g",
                        vec![],
                        types::Primitive::Integer64,
                        Block::new(
                            vec![
                                Call::new(
                                    types::Function::new(
                                        vec![],
                                        record_type.clone(),
                                        types::CallingConvention::Target
                                    ),
                                    Variable::new("f"),
                                    vec![],
                                    "x"
                                )
                                .into(),
                                DeconstructRecord::new(
                                    record_type.clone(),
                                    Variable::new("x"),
                                    0,
                                    "y"
                                )
                                .into()
                            ],
                            Return::new(types::Primitive::Integer64, Variable::new("y"))
                        ),
                        Default::default()
                    )]
                )),
                Ok(Module::new(
                    vec![],
                    vec![FunctionDeclaration::new(
                        "f",
                        types::Function::new(
                            vec![types::Pointer::new(record_type.clone()).into()],
                            void_type(),
                            types::CallingConvention::Target,
                        )
                    )],
                    vec![],
                    vec![FunctionDefinition::new(
                        "g",
                        vec![],
                        types::Primitive::Integer64,
                        Block::new(
                            vec![
                                AllocateStack::new(record_type.clone(), "x.p").into(),
                                Call::new(
                                    types::Function::new(
                                        vec![types::Pointer::new(record_type.clone()).into()],
                                        void_type(),
                                        types::CallingConvention::Target
                                    ),
                                    Variable::new("f"),
                                    vec![Variable::new("x.p").into()],
                                    "x.r"
                                )
                                .into(),
                                Load::new(record_type.clone(), Variable::new("x.p"), "x").into(),
                                DeconstructRecord::new(record_type, Variable::new("x"), 0, "y")
                                    .into()
                            ],
                            Return::new(types::Primitive::Integer64, Variable::new("y"))
                        ),
                        Default::default()
                    )]
                ))
            );
        }
    }

    mod function_definition {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn do_not_transform_compatible_function_definition() {
            let module = Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "g",
                    vec![],
                    types::Primitive::Integer64,
                    Block::new(
                        vec![],
                        Return::new(types::Primitive::Integer64, Primitive::Integer64(0)),
                    ),
                    Default::default(),
                )],
            );

            assert_eq!(transform_module(&module), Ok(module));
        }

        #[test]
        fn transform_function_definition() {
            let result_type = types::Record::new(vec![
                types::Primitive::Integer64.into(),
                types::Primitive::Integer64.into(),
                types::Primitive::Integer64.into(),
            ]);

            assert_eq!(
                transform_module(&Module::new(
                    vec![],
                    vec![],
                    vec![],
                    vec![FunctionDefinition::new(
                        "g",
                        vec![],
                        result_type.clone(),
                        Block::new(
                            vec![],
                            Return::new(result_type.clone(), Undefined::new(result_type.clone())),
                        ),
                        FunctionDefinitionOptions::new()
                            .set_calling_convention(types::CallingConvention::Target),
                    )],
                )),
                Ok(Module::new(
                    vec![],
                    vec![],
                    vec![],
                    vec![FunctionDefinition::new(
                        "g",
                        vec![Argument::new(
                            "g.p",
                            types::Pointer::new(result_type.clone())
                        )],
                        void_type(),
                        Block::new(
                            vec![Store::new(
                                result_type.clone(),
                                Undefined::new(result_type),
                                Variable::new("g.p")
                            )
                            .into()],
                            Return::new(void_type(), void_value()),
                        ),
                        FunctionDefinitionOptions::new()
                            .set_calling_convention(types::CallingConvention::Target),
                    )],
                ))
            );
        }
    }
}
