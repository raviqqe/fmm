mod context;
mod error;
mod function_declaration;
mod function_definition;
mod utilities;

use self::{context::Context, error::CCallingConventionError};
use super::type_check;
use crate::{ir::*, types};
use fnv::FnvHashMap;

// TODO Implement the complete C calling convention for all targets.
//
// Based on: https://refspecs.linuxfoundation.org/elf/x86_64-SysV-psABI.pdf
pub fn transform(module: &Module, word_bytes: usize) -> Result<Module, CCallingConventionError> {
    if ![4, 8].contains(&word_bytes) {
        return Err(CCallingConventionError::WordSize(word_bytes));
    }

    type_check::check(module)?;

    let context = Context::new(word_bytes);
    let mut changed_names = FnvHashMap::default();
    let mut function_declarations = vec![];

    for declaration in module.function_declarations() {
        if let Some(declaration) = function_declaration::transform(&context, declaration) {
            changed_names.insert(declaration.name().to_owned(), declaration.type_().clone());
            function_declarations.push(declaration);
        } else {
            function_declarations.push(declaration.clone());
        }
    }

    let mut function_definitions = vec![];

    for definition in module.function_definitions() {
        if let Some(definition) = function_definition::transform(&context, definition) {
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
            .map(|definition| transform_function_definition(definition, &changed_names))
            .collect(),
    );

    type_check::check(&module)?;

    Ok(module)
}

fn transform_function_definition(
    definition: &FunctionDefinition,
    names: &FnvHashMap<String, types::Function>,
) -> FunctionDefinition {
    FunctionDefinition::new(
        definition.name(),
        definition.arguments().to_vec(),
        definition.result_type().clone(),
        transform_block(definition.body(), names),
        definition.options().clone(),
    )
}

fn transform_block(block: &Block, names: &FnvHashMap<String, types::Function>) -> Block {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::void_type;
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
