mod context;
mod continuation_type;
mod error;
mod free_variable;
mod function_type;
mod if_;
mod source_function;
mod stack;
mod target_function;

use self::context::CpsContext;
use super::{name, type_check};
use crate::{ir::*, types::Type};
use error::CpsError;

pub fn transform(module: &Module, result_type: impl Into<Type>) -> Result<Module, CpsError> {
    name::check(module)?;
    type_check::check(module)?;

    let context = CpsContext::new(result_type.into());

    let module = if_::flatten(module);
    let module = source_function::transform(&context, &module)?;
    let module = target_function::transform(&context, &module)?;
    let module = function_type::transform(&module, context.result_type());

    name::check(&module)?;
    type_check::check(&module)?;

    Ok(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::format,
        types::{self, void_type, CallingConvention, Type},
    };
    use stack::type_;

    fn create_function_type(arguments: Vec<Type>, result: impl Into<Type>) -> types::Function {
        types::Function::new(arguments, result, CallingConvention::Source)
    }

    fn create_cps_function_type(arguments: Vec<Type>, result: impl Into<Type>) -> types::Function {
        types::Function::new(
            [
                type_(),
                types::Function::new(
                    vec![type_(), result.into()],
                    void_type(),
                    CallingConvention::Tail,
                )
                .into(),
            ]
            .into_iter()
            .chain(arguments)
            .collect(),
            void_type(),
            CallingConvention::Tail,
        )
    }

    fn create_function_definition(
        name: impl Into<String>,
        arguments: Vec<Argument>,
        result_type: impl Into<Type>,
        body: Block,
    ) -> FunctionDefinition {
        FunctionDefinition::new(name, arguments, result_type, body, Default::default())
    }

    fn test_transformation(module: &Module) {
        let one = transform(module, void_type()).unwrap();
        let other = transform(module, void_type()).unwrap();

        name::check(&one).unwrap();
        type_check::check(&one).unwrap();

        assert_eq!(one, other);
    }

    #[test]
    fn transform_empty_module() {
        test_transformation(&Module::new(vec![], vec![], vec![], vec![]));
    }

    #[test]
    fn transform_function_definition() {
        test_transformation(&Module::new(
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

        test_transformation(&Module::new(
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

        test_transformation(&Module::new(
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

        test_transformation(&Module::new(
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
        let cps_function_type = create_cps_function_type(
            vec![types::Primitive::Float64.into()],
            types::Primitive::Float64,
        );

        pretty_assertions::assert_eq!(
            transform(
                &Module::new(
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
                ),
                void_type()
            ),
            Ok(Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", cps_function_type.clone())],
                vec![],
                vec![FunctionDefinition::new(
                    "g",
                    vec![
                        Argument::new("_s", type_()),
                        Argument::new(
                            "_k",
                            types::Function::new(
                                vec![type_(), types::Primitive::Float64.into()],
                                void_type(),
                                CallingConvention::Tail,
                            )
                        ),
                    ],
                    void_type(),
                    Block::new(
                        vec![Call::new(
                            cps_function_type,
                            Variable::new("f"),
                            vec![
                                Variable::new("_s").into(),
                                Variable::new("_k").into(),
                                Primitive::Float64(42.0).into()
                            ],
                            "_k_0",
                        )
                        .into()],
                        Return::new(void_type(), Variable::new("_k_0")),
                    ),
                    FunctionDefinitionOptions::new()
                        .set_calling_convention(CallingConvention::Tail)
                )],
            ))
        );
    }

    #[test]
    fn transform_call_with_continuation_with_free_variable() {
        let function_type = create_function_type(vec![], types::Primitive::PointerInteger);

        test_transformation(&Module::new(
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
    fn transform_call_with_continuation_with_free_variable_from_if() {
        let function_type = create_function_type(vec![], types::Primitive::PointerInteger);
        let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

        test_transformation(&Module::new(
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
                        If::new(
                            pointer_type.clone(),
                            Primitive::Boolean(true),
                            Block::new(vec![], Branch::new(pointer_type, Variable::new("p"))),
                            Block::new(vec![], TerminalInstruction::Unreachable),
                            "q",
                        )
                        .into(),
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
                            Variable::new("q"),
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

    mod if_ {
        use super::*;

        #[test]
        fn transform_call_in_block() {
            let function_type = create_function_type(
                vec![types::Primitive::Float64.into()],
                types::Primitive::Float64,
            );

            test_transformation(&Module::new(
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

            insta::assert_snapshot!(format::format_module(
                &transform(
                    &Module::new(
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
                    ),
                    void_type()
                )
                .unwrap()
            ));
        }

        #[test]
        fn transform_tail_call_in_block() {
            let function_type = create_function_type(
                vec![types::Primitive::Float64.into()],
                types::Primitive::Float64,
            );

            test_transformation(&Module::new(
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

            insta::assert_snapshot!(format::format_module(
                &transform(
                    &Module::new(
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
                    ),
                    void_type()
                )
                .unwrap()
            ));
        }

        #[test]
        fn transform_call_in_block_and_call_after_if() {
            let function_type = create_function_type(
                vec![types::Primitive::PointerInteger.into()],
                types::Primitive::PointerInteger,
            );

            test_transformation(&Module::new(
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

            insta::assert_snapshot!(format::format_module(
                &transform(
                    &Module::new(
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
                                            Branch::new(
                                                types::Primitive::Float64,
                                                Variable::new("x")
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
                                    .into()
                                ],
                                Return::new(types::Primitive::Float64, Variable::new("z")),
                            ),
                        )],
                    ),
                    void_type()
                )
                .unwrap()
            ));
        }

        #[test]
        fn transform_call_in_block_with_multiple_free_variables() {
            let function_type = create_function_type(
                vec![types::Primitive::Float64.into()],
                types::Primitive::Float64,
            );

            test_transformation(&Module::new(
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

            test_transformation(&Module::new(
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

            test_transformation(&Module::new(
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

            test_transformation(&Module::new(
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

            test_transformation(&Module::new(
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

            test_transformation(&Module::new(
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
