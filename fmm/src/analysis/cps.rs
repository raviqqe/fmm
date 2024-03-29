mod context;
mod continuation_type;
mod environment_inference;
mod error;
mod function_type;
mod if_;
mod source_function;
mod stack;
mod target_function;
mod utility;

use self::context::Context;
use crate::{ir::*, types::Type};
use error::CpsError;

pub fn transform(module: &mut Module, result_type: impl Into<Type>) -> Result<(), CpsError> {
    let context = Context::new(result_type.into());

    environment_inference::transform(module);
    if_::flatten(module);
    source_function::transform(&context, module)?;
    target_function::transform(&context, module)?;
    function_type::transform(module, context.result_type())?;
    stack::define_utility_functions(module)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::{format, validation},
        types::{self, void_type, CallingConvention, Type},
    };

    fn create_function_type(arguments: Vec<Type>, result: impl Into<Type>) -> types::Function {
        types::Function::new(arguments, result, CallingConvention::Source)
    }

    fn create_function_definition(
        name: impl Into<String>,
        arguments: Vec<Argument>,
        result_type: impl Into<Type>,
        body: Block,
    ) -> FunctionDefinition {
        FunctionDefinition::new(name, arguments, result_type, body, Default::default())
    }

    fn transform_module(mut module: Module) -> Module {
        validation::validate(&module).unwrap();

        let mut other = module.clone();

        transform(&mut module, void_type()).unwrap();
        transform(&mut other, void_type()).unwrap();

        validation::validate(&module).unwrap();

        assert_eq!(module, other);

        module
    }

    #[test]
    fn transform_empty_module() {
        transform_module(Module::new(vec![], vec![], vec![], vec![]));
    }

    #[test]
    fn transform_function_definition() {
        transform_module(Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![],
                types::Primitive::Float64,
                Block::new(
                    vec![],
                    Return::new(types::Primitive::Float64, Primitive::Float64(42.0)),
                ),
            )],
        ));
    }

    #[test]
    fn transform_call() {
        let function_type = create_function_type(
            vec![types::Primitive::Float64.into()],
            types::Primitive::Float64,
        );

        transform_module(Module::new(
            vec![],
            vec![FunctionDeclaration::new("f", function_type.clone())],
            vec![],
            vec![create_function_definition(
                "g",
                vec![],
                types::Primitive::Float64,
                Block::new(
                    vec![Call::new(
                        function_type,
                        Variable::new("f"),
                        vec![Primitive::Float64(42.0).into()],
                        "x",
                    )
                    .into()],
                    Return::new(types::Primitive::Float64, Variable::new("x")),
                ),
            )],
        ));
    }

    #[test]
    fn transform_instruction_after_call() {
        let function_type =
            create_function_type(vec![], types::Pointer::new(types::Primitive::Float64));

        transform_module(Module::new(
            vec![],
            vec![FunctionDeclaration::new("f", function_type.clone())],
            vec![],
            vec![create_function_definition(
                "g",
                vec![],
                types::Primitive::Float64,
                Block::new(
                    vec![
                        Call::new(function_type, Variable::new("f"), vec![], "x").into(),
                        Load::new(types::Primitive::Float64, Variable::new("x"), "y").into(),
                    ],
                    Return::new(types::Primitive::Float64, Variable::new("y")),
                ),
            )],
        ));
    }

    #[test]
    fn transform_two_calls() {
        let function_type = create_function_type(
            vec![types::Primitive::PointerInteger.into()],
            types::Primitive::PointerInteger,
        );

        transform_module(Module::new(
            vec![],
            vec![FunctionDeclaration::new("f", function_type.clone())],
            vec![],
            vec![create_function_definition(
                "g",
                vec![],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![
                        Call::new(
                            function_type.clone(),
                            Variable::new("f"),
                            vec![Primitive::PointerInteger(42).into()],
                            "x",
                        )
                        .into(),
                        Call::new(
                            function_type,
                            Variable::new("f"),
                            vec![Variable::new("x").into()],
                            "y",
                        )
                        .into(),
                    ],
                    Return::new(types::Primitive::PointerInteger, Variable::new("y")),
                ),
            )],
        ));
    }

    #[test]
    fn transform_tail_call() {
        let function_type = create_function_type(
            vec![types::Primitive::Float64.into()],
            types::Primitive::Float64,
        );

        insta::assert_snapshot!(format::format_module(&transform_module(Module::new(
            vec![],
            vec![FunctionDeclaration::new("f", function_type.clone())],
            vec![],
            vec![create_function_definition(
                "g",
                vec![],
                types::Primitive::Float64,
                Block::new(
                    vec![Call::new(
                        function_type,
                        Variable::new("f"),
                        vec![Primitive::Float64(42.0).into()],
                        "x",
                    )
                    .into()],
                    Return::new(types::Primitive::Float64, Variable::new("x")),
                ),
            )],
        ))));
    }

    #[test]
    fn transform_call_with_continuation_with_free_variable() {
        let function_type = create_function_type(vec![], types::Primitive::PointerInteger);

        transform_module(Module::new(
            vec![],
            vec![FunctionDeclaration::new("f", function_type.clone())],
            vec![],
            vec![create_function_definition(
                "g",
                vec![Argument::new(
                    "p",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![
                        Load::new(types::Primitive::PointerInteger, Variable::new("p"), "x").into(),
                        Call::new(function_type, Variable::new("f"), vec![], "y").into(),
                        Store::new(
                            types::Primitive::PointerInteger,
                            Variable::new("x"),
                            Variable::new("p"),
                        )
                        .into(),
                        Store::new(
                            types::Primitive::PointerInteger,
                            Variable::new("y"),
                            Variable::new("p"),
                        )
                        .into(),
                    ],
                    Return::new(
                        types::Primitive::PointerInteger,
                        ArithmeticOperation::new(
                            types::Primitive::PointerInteger,
                            ArithmeticOperator::Add,
                            Variable::new("x"),
                            Variable::new("y"),
                        ),
                    ),
                ),
            )],
        ));
    }

    #[test]
    fn transform_free_variable_between_two_calls() {
        transform_module(Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![],
                types::Primitive::Float64,
                Block::new(
                    vec![
                        Call::new(
                            types::Function::new(
                                vec![],
                                types::Primitive::Float64,
                                types::CallingConvention::Source,
                            ),
                            Variable::new("f"),
                            vec![],
                            "x",
                        )
                        .into(),
                        Load::new(
                            types::Primitive::Float64,
                            Undefined::new(types::Pointer::new(types::Primitive::Float64)),
                            "y",
                        )
                        .into(),
                        Call::new(
                            types::Function::new(
                                vec![],
                                types::Primitive::Float64,
                                types::CallingConvention::Source,
                            ),
                            Variable::new("f"),
                            vec![],
                            "z",
                        )
                        .into(),
                    ],
                    Return::new(
                        types::Primitive::Float64,
                        ArithmeticOperation::new(
                            types::Primitive::Float64,
                            ArithmeticOperator::Add,
                            Variable::new("y"),
                            Variable::new("z"),
                        ),
                    ),
                ),
                Default::default(),
            )],
        ));
    }

    mod if_ {
        use super::*;

        #[test]
        fn transform_call_in_block() {
            let function_type = create_function_type(
                vec![types::Primitive::Float64.into()],
                types::Primitive::Float64,
            );

            transform_module(Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", function_type.clone())],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![If::new(
                            types::Primitive::Float64,
                            Primitive::Boolean(true),
                            Block::new(
                                vec![Call::new(
                                    function_type,
                                    Variable::new("f"),
                                    vec![Primitive::Float64(42.0).into()],
                                    "x",
                                )
                                .into()],
                                Branch::new(types::Primitive::Float64, Variable::new("x")),
                            ),
                            Block::new(vec![], TerminalInstruction::Unreachable),
                            "y",
                        )
                        .into()],
                        Return::new(types::Primitive::Float64, Variable::new("y")),
                    ),
                )],
            ));
        }

        #[test]
        fn transform_call_before_branch_into_tail_call() {
            let function_type = create_function_type(
                vec![types::Primitive::Float64.into()],
                types::Primitive::Float64,
            );

            insta::assert_snapshot!(format::format_module(&transform_module(Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", function_type.clone())],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![If::new(
                            types::Primitive::Float64,
                            Primitive::Boolean(true),
                            Block::new(
                                vec![Call::new(
                                    function_type,
                                    Variable::new("f"),
                                    vec![Primitive::Float64(42.0).into()],
                                    "x",
                                )
                                .into()],
                                Branch::new(types::Primitive::Float64, Variable::new("x")),
                            ),
                            Block::new(vec![], TerminalInstruction::Unreachable),
                            "y",
                        )
                        .into()],
                        Return::new(types::Primitive::Float64, Variable::new("y")),
                    ),
                )],
            ))));
        }

        #[test]
        fn transform_tail_call_in_block() {
            let function_type = create_function_type(
                vec![types::Primitive::Float64.into()],
                types::Primitive::Float64,
            );

            transform_module(Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", function_type.clone())],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![If::new(
                            void_type(),
                            Primitive::Boolean(true),
                            Block::new(
                                vec![Call::new(
                                    function_type,
                                    Variable::new("f"),
                                    vec![Primitive::Float64(42.0).into()],
                                    "x",
                                )
                                .into()],
                                Return::new(types::Primitive::Float64, Variable::new("x")),
                            ),
                            Block::new(vec![], TerminalInstruction::Unreachable),
                            "_",
                        )
                        .into()],
                        TerminalInstruction::Unreachable,
                    ),
                )],
            ));
        }

        #[test]
        fn transform_non_tail_call_in_block() {
            let function_type = create_function_type(
                vec![types::Primitive::Float64.into()],
                types::Primitive::Float64,
            );

            insta::assert_snapshot!(format::format_module(&transform_module(Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", function_type.clone())],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![If::new(
                            types::Primitive::Float64,
                            Primitive::Boolean(true),
                            Block::new(
                                vec![Call::new(
                                    function_type,
                                    Variable::new("f"),
                                    vec![Primitive::Float64(42.0).into()],
                                    "x",
                                )
                                .into()],
                                Branch::new(types::Primitive::Float64, Variable::new("x")),
                            ),
                            Block::new(vec![], TerminalInstruction::Unreachable),
                            "y",
                        )
                        .into()],
                        Return::new(
                            types::Primitive::Float64,
                            ArithmeticOperation::new(
                                types::Primitive::Float64,
                                ArithmeticOperator::Add,
                                Variable::new("y"),
                                Variable::new("y")
                            )
                        ),
                    ),
                )],
            ))));
        }

        #[test]
        fn transform_call_in_block_and_call_after_if() {
            let function_type = create_function_type(
                vec![types::Primitive::PointerInteger.into()],
                types::Primitive::PointerInteger,
            );

            transform_module(Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", function_type.clone())],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![
                            If::new(
                                types::Primitive::PointerInteger,
                                Primitive::Boolean(true),
                                Block::new(
                                    vec![Call::new(
                                        function_type.clone(),
                                        Variable::new("f"),
                                        vec![Primitive::PointerInteger(42).into()],
                                        "x",
                                    )
                                    .into()],
                                    Branch::new(
                                        types::Primitive::PointerInteger,
                                        Variable::new("x"),
                                    ),
                                ),
                                Block::new(vec![], TerminalInstruction::Unreachable),
                                "y",
                            )
                            .into(),
                            Call::new(
                                function_type,
                                Variable::new("f"),
                                vec![Variable::new("y").into()],
                                "z",
                            )
                            .into(),
                        ],
                        Return::new(types::Primitive::PointerInteger, Variable::new("z")),
                    ),
                )],
            ));
        }

        #[test]
        fn transform_call_after_preserved_if() {
            let function_type = create_function_type(
                vec![types::Primitive::Float64.into()],
                types::Primitive::Float64,
            );

            insta::assert_snapshot!(format::format_module(&transform_module(Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", function_type.clone())],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![
                            If::new(
                                types::Primitive::Float64,
                                Primitive::Boolean(true),
                                Block::new(
                                    vec![Load::new(
                                        types::Primitive::Float64,
                                        Undefined::new(types::Pointer::new(
                                            types::Primitive::Float64
                                        )),
                                        "x"
                                    )
                                    .into()],
                                    Branch::new(types::Primitive::Float64, Variable::new("x")),
                                ),
                                Block::new(vec![], TerminalInstruction::Unreachable),
                                "y",
                            )
                            .into(),
                            Call::new(
                                function_type,
                                Variable::new("f"),
                                vec![Variable::new("y").into()],
                                "z",
                            )
                            .into()
                        ],
                        Return::new(types::Primitive::Float64, Variable::new("z")),
                    ),
                )],
            ))));
        }

        #[test]
        fn transform_call_in_block_with_multiple_free_variables() {
            let function_type = create_function_type(
                vec![types::Primitive::Float64.into()],
                types::Primitive::Float64,
            );

            transform_module(Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", function_type.clone())],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![
                        Argument::new("a", types::Primitive::Float64),
                        Argument::new("b", types::Primitive::Float64),
                    ],
                    types::Primitive::Float64,
                    Block::new(
                        vec![If::new(
                            types::Primitive::Float64,
                            Primitive::Boolean(true),
                            Block::new(
                                vec![Call::new(
                                    function_type,
                                    Variable::new("f"),
                                    vec![Primitive::Float64(42.0).into()],
                                    "x",
                                )
                                .into()],
                                Branch::new(types::Primitive::Float64, Variable::new("x")),
                            ),
                            Block::new(vec![], TerminalInstruction::Unreachable),
                            "y",
                        )
                        .into()],
                        Return::new(
                            types::Primitive::Float64,
                            ArithmeticOperation::new(
                                types::Primitive::Float64,
                                ArithmeticOperator::Add,
                                ArithmeticOperation::new(
                                    types::Primitive::Float64,
                                    ArithmeticOperator::Add,
                                    Variable::new("a"),
                                    Variable::new("b"),
                                ),
                                Variable::new("y"),
                            ),
                        ),
                    ),
                )],
            ));
        }

        #[test]
        fn transform_call_in_block_with_continuation_with_instructions() {
            let function_type = create_function_type(vec![], types::Primitive::PointerInteger);

            transform_module(Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", function_type.clone())],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![Argument::new(
                        "p",
                        types::Pointer::new(types::Primitive::PointerInteger),
                    )],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![
                            Load::new(types::Primitive::PointerInteger, Variable::new("p"), "x")
                                .into(),
                            If::new(
                                types::Primitive::PointerInteger,
                                Primitive::Boolean(true),
                                Block::new(
                                    vec![Call::new(function_type, Variable::new("f"), vec![], "a")
                                        .into()],
                                    Branch::new(
                                        types::Primitive::PointerInteger,
                                        Variable::new("a"),
                                    ),
                                ),
                                Block::new(
                                    vec![],
                                    Branch::new(
                                        types::Primitive::PointerInteger,
                                        Primitive::PointerInteger(2),
                                    ),
                                ),
                                "y",
                            )
                            .into(),
                            Store::new(
                                types::Primitive::PointerInteger,
                                Variable::new("x"),
                                Variable::new("p"),
                            )
                            .into(),
                            Store::new(
                                types::Primitive::PointerInteger,
                                Variable::new("y"),
                                Variable::new("p"),
                            )
                            .into(),
                        ],
                        Return::new(
                            types::Primitive::PointerInteger,
                            ArithmeticOperation::new(
                                types::Primitive::PointerInteger,
                                ArithmeticOperator::Add,
                                Variable::new("x"),
                                Variable::new("y"),
                            ),
                        ),
                    ),
                )],
            ));
        }

        #[test]
        fn transform_if_with_source_call_and_preserved_if() {
            let function_type = create_function_type(
                vec![types::Primitive::Float64.into()],
                types::Primitive::Float64,
            );

            transform_module(Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", function_type.clone())],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![If::new(
                            types::Primitive::Float64,
                            Primitive::Boolean(true),
                            Block::new(
                                vec![
                                    If::new(
                                        types::Primitive::Float64,
                                        Primitive::Boolean(true),
                                        Block::new(vec![], TerminalInstruction::Unreachable),
                                        Block::new(vec![], TerminalInstruction::Unreachable),
                                        "",
                                    )
                                    .into(),
                                    Call::new(
                                        function_type,
                                        Variable::new("f"),
                                        vec![Primitive::Float64(42.0).into()],
                                        "x",
                                    )
                                    .into(),
                                ],
                                Branch::new(types::Primitive::Float64, Variable::new("x")),
                            ),
                            Block::new(vec![], TerminalInstruction::Unreachable),
                            "y",
                        )
                        .into()],
                        Return::new(types::Primitive::Float64, Variable::new("y")),
                    ),
                )],
            ));
        }
    }

    mod target_function_definition {
        use super::*;

        #[test]
        fn transform_with_no_argument() {
            let function_type = create_function_type(vec![], types::Primitive::Float64);

            transform_module(Module::new(
                vec![],
                vec![FunctionDeclaration::new("g", function_type.clone())],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![Call::new(function_type, Variable::new("g"), vec![], "x").into()],
                        Return::new(types::Primitive::Float64, Variable::new("x")),
                    ),
                    FunctionDefinitionOptions::new()
                        .set_calling_convention(CallingConvention::Target),
                )],
            ));
        }

        #[test]
        fn transform_one_argument() {
            let function_type = create_function_type(
                vec![types::Primitive::Float64.into()],
                types::Primitive::Float64,
            );

            transform_module(Module::new(
                vec![],
                vec![FunctionDeclaration::new("g", function_type.clone())],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![Call::new(
                            function_type,
                            Variable::new("g"),
                            vec![Primitive::Float64(42.0).into()],
                            "x",
                        )
                        .into()],
                        Return::new(types::Primitive::Float64, Variable::new("x")),
                    ),
                    FunctionDefinitionOptions::new()
                        .set_calling_convention(CallingConvention::Target),
                )],
            ));
        }

        #[test]
        fn transform_two_argument() {
            let function_type = create_function_type(
                vec![
                    types::Primitive::Float64.into(),
                    types::Primitive::Integer64.into(),
                ],
                types::Primitive::Float64,
            );

            transform_module(Module::new(
                vec![],
                vec![FunctionDeclaration::new("g", function_type.clone())],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![Call::new(
                            function_type,
                            Variable::new("g"),
                            vec![
                                Primitive::Float64(42.0).into(),
                                Primitive::Integer64(42).into(),
                            ],
                            "x",
                        )
                        .into()],
                        Return::new(types::Primitive::Float64, Variable::new("x")),
                    ),
                    FunctionDefinitionOptions::new()
                        .set_calling_convention(CallingConvention::Target),
                )],
            ));
        }
    }
}
