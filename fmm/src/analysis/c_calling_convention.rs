mod call;
mod context;
mod error;
mod function_declaration;
mod function_definition;
mod type_;

use self::{context::Context, error::CCallingConventionError};
use super::{type_check, type_conversion};
use crate::{ir::*, types::Type};

// TODO Implement the complete C calling convention for all targets.
//
// Based on: https://refspecs.linuxfoundation.org/elf/x86_64-SysV-psABI.pdf
pub fn transform(module: &Module, word_bytes: usize) -> Result<Module, CCallingConventionError> {
    if ![4, 8].contains(&word_bytes) {
        return Err(CCallingConventionError::WordSize(word_bytes));
    }

    type_check::check(module)?;

    let context = Context::new(word_bytes);
    let module = Module::new(
        module.variable_declarations().to_vec(),
        module
            .function_declarations()
            .iter()
            .map(|declaration| function_declaration::transform(&context, declaration))
            .collect(),
        module.variable_definitions().to_vec(),
        module
            .function_definitions()
            .iter()
            .map(|definition| function_definition::transform(&context, definition))
            .map(|definition| call::transform_function_definition(&context, &definition))
            .collect::<Result<_, _>>()?,
    );

    let module = type_conversion::convert(&module, &|type_| match type_ {
        Type::Function(function) => type_::transform_function(&context, function).into(),
        _ => type_.clone(),
    });

    type_check::check(&module)?;

    Ok(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{self, void_type};
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

    mod variable_declaration {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn transform() {
            let result_type = types::Record::new(vec![
                types::Primitive::Integer64.into(),
                types::Primitive::Integer64.into(),
                types::Primitive::Integer64.into(),
            ]);

            assert_eq!(
                transform_module(&Module::new(
                    vec![VariableDeclaration::new(
                        "x",
                        types::Function::new(
                            vec![],
                            result_type.clone(),
                            types::CallingConvention::Target,
                        )
                    )],
                    vec![],
                    vec![],
                    vec![]
                )),
                Ok(Module::new(
                    vec![VariableDeclaration::new(
                        "x",
                        types::Function::new(
                            vec![types::Pointer::new(result_type).into()],
                            void_type(),
                            types::CallingConvention::Target,
                        )
                    )],
                    vec![],
                    vec![],
                    vec![]
                ))
            );
        }
    }

    mod function_declaration {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn transform_compatible_function_declaration() {
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
        fn transform_argument_of_function_declaration() {
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
                            vec![result_type.clone().into()],
                            void_type(),
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
        fn transform_result_of_function_declaration() {
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
    }

    mod function_definition {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn transform_compatible_function_definition() {
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
                            "f_c_pointer",
                            types::Pointer::new(record_type.clone())
                        )],
                        void_type(),
                        Block::new(
                            vec![Store::new(
                                record_type.clone(),
                                Undefined::new(record_type),
                                Variable::new("f_c_pointer")
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
                                "f_c_pointer",
                                types::Pointer::new(record_type.clone())
                            )],
                            void_type(),
                            Block::new(
                                vec![Store::new(
                                    record_type.clone(),
                                    Undefined::new(record_type.clone()),
                                    Variable::new("f_c_pointer")
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
                                    DeconstructRecord::new(record_type, Variable::new("x"), 0, "y")
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

        #[test]
        fn transform_argument_in_call_in_function_definition() {
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
                            vec![record_type.clone().into()],
                            types::Primitive::Integer64,
                            types::CallingConvention::Target,
                        ),
                    )],
                    vec![],
                    vec![FunctionDefinition::new(
                        "g",
                        vec![],
                        types::Primitive::Integer64,
                        Block::new(
                            vec![Call::new(
                                types::Function::new(
                                    vec![record_type.clone().into()],
                                    types::Primitive::Integer64,
                                    types::CallingConvention::Target,
                                ),
                                Variable::new("f"),
                                vec![Undefined::new(record_type.clone()).into()],
                                "x",
                            )
                            .into()],
                            Return::new(types::Primitive::Integer64, Variable::new("x")),
                        ),
                        FunctionDefinitionOptions::new()
                            .set_calling_convention(types::CallingConvention::Target),
                    )],
                )),
                Ok(Module::new(
                    vec![],
                    vec![FunctionDeclaration::new(
                        "f",
                        types::Function::new(
                            vec![types::Pointer::new(record_type.clone()).into()],
                            types::Primitive::Integer64,
                            types::CallingConvention::Target,
                        ),
                    )],
                    vec![],
                    vec![FunctionDefinition::new(
                        "g",
                        vec![],
                        types::Primitive::Integer64,
                        Block::new(
                            vec![
                                AllocateStack::new(record_type.clone(), "x_c_0").into(),
                                Store::new(
                                    record_type.clone(),
                                    Undefined::new(record_type.clone()),
                                    Variable::new("x_c_0")
                                )
                                .into(),
                                Call::new(
                                    types::Function::new(
                                        vec![types::Pointer::new(record_type).into()],
                                        types::Primitive::Integer64,
                                        types::CallingConvention::Target,
                                    ),
                                    Variable::new("f"),
                                    vec![Variable::new("x_c_0").into()],
                                    "x",
                                )
                                .into()
                            ],
                            Return::new(types::Primitive::Integer64, Variable::new("x")),
                        ),
                        FunctionDefinitionOptions::new()
                            .set_calling_convention(types::CallingConvention::Target),
                    )],
                ))
            );
        }

        #[test]
        fn transform_result_in_call_in_function_definition() {
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

        #[test]
        fn transform_argument_and_result_in_call_in_function_definition() {
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
                            vec![record_type.clone().into()],
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
                                        vec![record_type.clone().into()],
                                        record_type.clone(),
                                        types::CallingConvention::Target
                                    ),
                                    Variable::new("f"),
                                    vec![Undefined::new(record_type.clone()).into()],
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
                            vec![
                                types::Pointer::new(record_type.clone()).into(),
                                types::Pointer::new(record_type.clone()).into()
                            ],
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
                                Store::new(
                                    record_type.clone(),
                                    Undefined::new(record_type.clone()),
                                    Variable::new("x_c_0")
                                )
                                .into(),
                                AllocateStack::new(record_type.clone(), "x_c_1").into(),
                                Call::new(
                                    types::Function::new(
                                        vec![
                                            types::Pointer::new(record_type.clone()).into(),
                                            types::Pointer::new(record_type.clone()).into()
                                        ],
                                        void_type(),
                                        types::CallingConvention::Target
                                    ),
                                    Variable::new("f"),
                                    vec![
                                        Variable::new("x_c_1").into(),
                                        Variable::new("x_c_0").into()
                                    ],
                                    "x_c_2"
                                )
                                .into(),
                                Load::new(record_type.clone(), Variable::new("x_c_1"), "x").into(),
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

        #[test]
        fn transform_argument_and_result_in_call_with_compatible_argument() {
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
                            vec![
                                types::Primitive::PointerInteger.into(),
                                record_type.clone().into()
                            ],
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
                                        vec![
                                            types::Primitive::PointerInteger.into(),
                                            record_type.clone().into()
                                        ],
                                        record_type.clone(),
                                        types::CallingConvention::Target
                                    ),
                                    Variable::new("f"),
                                    vec![
                                        Primitive::PointerInteger(42).into(),
                                        Undefined::new(record_type.clone()).into()
                                    ],
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
                            vec![
                                types::Pointer::new(record_type.clone()).into(),
                                types::Primitive::PointerInteger.into(),
                                types::Pointer::new(record_type.clone()).into()
                            ],
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
                                Store::new(
                                    record_type.clone(),
                                    Undefined::new(record_type.clone()),
                                    Variable::new("x_c_0")
                                )
                                .into(),
                                AllocateStack::new(record_type.clone(), "x_c_1").into(),
                                Call::new(
                                    types::Function::new(
                                        vec![
                                            types::Pointer::new(record_type.clone()).into(),
                                            types::Primitive::PointerInteger.into(),
                                            types::Pointer::new(record_type.clone()).into()
                                        ],
                                        void_type(),
                                        types::CallingConvention::Target
                                    ),
                                    Variable::new("f"),
                                    vec![
                                        Variable::new("x_c_1").into(),
                                        Primitive::PointerInteger(42).into(),
                                        Variable::new("x_c_0").into()
                                    ],
                                    "x_c_2"
                                )
                                .into(),
                                Load::new(record_type.clone(), Variable::new("x_c_1"), "x").into(),
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
}
