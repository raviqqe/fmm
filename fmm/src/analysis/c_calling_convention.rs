mod context;
mod error;
mod function_declaration;
mod function_definition;
mod function_type;
mod utilities;

use self::{context::Context, error::CCallingConventionError};
use super::type_check;
use crate::{
    build::{self, InstructionBuilder, NameGenerator, TypedExpression},
    ir::*,
    types,
};
use fnv::FnvHashMap;
use std::rc::Rc;

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
            .map(|definition| transform_function_definition(&context, definition))
            .collect(),
    );

    type_check::check(&module)?;

    Ok(module)
}

fn transform_function_definition(
    context: &Context,
    definition: &FunctionDefinition,
) -> FunctionDefinition {
    FunctionDefinition::new(
        definition.name(),
        definition.arguments().to_vec(),
        definition.result_type().clone(),
        transform_block(context, definition.body()),
        definition.options().clone(),
    )
}

fn transform_block(context: &Context, block: &Block) -> Block {
    let mut instructions = vec![];

    for instruction in block.instructions() {
        instructions.extend(transform_instruction(context, instruction));
    }

    Block::new(instructions, block.terminal_instruction().clone())
}

fn transform_instruction(context: &Context, instruction: &Instruction) -> Vec<Instruction> {
    match instruction {
        Instruction::Call(call)
            if call.type_().calling_convention() == types::CallingConvention::Target =>
        {
            match call.function() {
                // TODO Support complex expressions.
                Expression::Variable(variable) => {
                    let builder = InstructionBuilder::new(Rc::new(
                        NameGenerator::new(format!("{}_c_", call.name())).into(),
                    ));
                    let function_type = call.type_();
                    let pointer = builder.allocate_stack(function_type.result().clone());

                    builder
                        .call(
                            build::variable(
                                variable.name(),
                                function_type::transform(context, function_type).unwrap(),
                            ),
                            call.arguments()
                                .iter()
                                .zip(function_type.arguments())
                                .map(|(argument, type_)| {
                                    TypedExpression::new(argument.clone(), type_.clone())
                                })
                                .chain([pointer.clone().into()])
                                .collect(),
                        )
                        .unwrap();

                    builder.add_instruction(Load::new(
                        function_type.result().clone(),
                        pointer.expression().clone(),
                        call.name(),
                    ));

                    builder.into_instructions()
                }
                _ => vec![call.clone().into()],
            }
        }
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
                                AllocateStack::new(record_type.clone(), "x_c_0").into(),
                                Call::new(
                                    types::Function::new(
                                        vec![types::Pointer::new(record_type.clone()).into()],
                                        void_type(),
                                        types::CallingConvention::Target
                                    ),
                                    Variable::new("f"),
                                    vec![Variable::new("x_c_0").into()],
                                    "x_c_1"
                                )
                                .into(),
                                Load::new(record_type.clone(), Variable::new("x_c_0"), "x").into(),
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
            let record_type = types::Record::new(vec![
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
                        "f",
                        vec![],
                        record_type.clone(),
                        Block::new(
                            vec![],
                            Return::new(record_type.clone(), Undefined::new(record_type.clone())),
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
                        "f",
                        vec![Argument::new(
                            "f.p",
                            types::Pointer::new(record_type.clone())
                        )],
                        void_type(),
                        Block::new(
                            vec![Store::new(
                                record_type.clone(),
                                Undefined::new(record_type),
                                Variable::new("f.p")
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

        #[test]
        fn transform_function_definition_with_call() {
            let record_type = types::Record::new(vec![
                types::Primitive::Integer64.into(),
                types::Primitive::Integer64.into(),
                types::Primitive::Integer64.into(),
            ]);

            assert_eq!(
                transform_module(&Module::new(
                    vec![],
                    vec![],
                    vec![],
                    vec![
                        FunctionDefinition::new(
                            "f",
                            vec![],
                            record_type.clone(),
                            Block::new(
                                vec![],
                                Return::new(
                                    record_type.clone(),
                                    Undefined::new(record_type.clone())
                                ),
                            ),
                            FunctionDefinitionOptions::new()
                                .set_calling_convention(types::CallingConvention::Target),
                        ),
                        FunctionDefinition::new(
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
                                Return::new(types::Primitive::Integer64, Variable::new("y")),
                            ),
                            FunctionDefinitionOptions::new()
                                .set_calling_convention(types::CallingConvention::Target),
                        )
                    ],
                )),
                Ok(Module::new(
                    vec![],
                    vec![],
                    vec![],
                    vec![
                        FunctionDefinition::new(
                            "f",
                            vec![Argument::new(
                                "f.p",
                                types::Pointer::new(record_type.clone())
                            )],
                            void_type(),
                            Block::new(
                                vec![Store::new(
                                    record_type.clone(),
                                    Undefined::new(record_type.clone()),
                                    Variable::new("f.p")
                                )
                                .into()],
                                Return::new(void_type(), void_value()),
                            ),
                            FunctionDefinitionOptions::new()
                                .set_calling_convention(types::CallingConvention::Target),
                        ),
                        FunctionDefinition::new(
                            "g",
                            vec![],
                            types::Primitive::Integer64,
                            Block::new(
                                vec![
                                    AllocateStack::new(record_type.clone(), "x_c_0").into(),
                                    Call::new(
                                        types::Function::new(
                                            vec![types::Pointer::new(record_type.clone()).into()],
                                            void_type(),
                                            types::CallingConvention::Target
                                        ),
                                        Variable::new("f"),
                                        vec![Variable::new("x_c_0").into()],
                                        "x_c_1"
                                    )
                                    .into(),
                                    Load::new(record_type.clone(), Variable::new("x_c_0"), "x")
                                        .into(),
                                    DeconstructRecord::new(
                                        record_type.clone(),
                                        Variable::new("x"),
                                        0,
                                        "y"
                                    )
                                    .into(),
                                ],
                                Return::new(types::Primitive::Integer64, Variable::new("y")),
                            ),
                            FunctionDefinitionOptions::new()
                                .set_calling_convention(types::CallingConvention::Target),
                        )
                    ],
                ))
            );
        }
    }
}
