mod context;
mod continuation_type_compiler;
mod error;
mod free_variable_collector;
mod function_type_transformer;
mod if_flattener;
mod native_function_transformer;
mod source_function_transformer;
mod stack;

use self::context::CpsContext;
use super::check_types;
use crate::{ir::*, types::Type};
use error::CpsTransformationError;

pub fn transform_to_cps(
    module: &Module,
    result_type: impl Into<Type>,
) -> Result<Module, CpsTransformationError> {
    check_types(module)?;

    let context = CpsContext::new(result_type.into());

    let module = if_flattener::flatten(module);
    let module = source_function_transformer::transform(&context, &module)?;
    let module = native_function_transformer::transform(&context, &module)?;
    let module = function_type_transformer::transform(&module, context.result_type());

    check_types(&module)?;

    Ok(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::{check_types, format_module},
        types::{self, CallingConvention, Type, VOID_TYPE},
    };
    use stack::STACK_TYPE;

    fn create_function_type(arguments: Vec<Type>, result: impl Into<Type>) -> types::Function {
        types::Function::new(arguments, result, CallingConvention::Source)
    }

    fn create_cps_function_type(arguments: Vec<Type>, result: impl Into<Type>) -> types::Function {
        types::Function::new(
            [
                STACK_TYPE.clone(),
                types::Function::new(
                    vec![STACK_TYPE.clone(), result.into()],
                    VOID_TYPE.clone(),
                    CallingConvention::Tail,
                )
                .into(),
            ]
            .into_iter()
            .chain(arguments)
            .collect(),
            VOID_TYPE.clone(),
            CallingConvention::Tail,
        )
    }

    fn create_function_definition(
        name: impl Into<String>,
        arguments: Vec<Argument>,
        body: Block,
        result_type: impl Into<Type>,
    ) -> FunctionDefinition {
        FunctionDefinition::new(
            name,
            arguments,
            body,
            result_type,
            CallingConvention::Source,
            Linkage::Internal,
        )
    }

    fn test_transformation(module: &Module) {
        let one = transform_to_cps(module, VOID_TYPE.clone()).unwrap();
        let other = transform_to_cps(module, VOID_TYPE.clone()).unwrap();

        check_types(&one).unwrap();

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
                Block::new(
                    vec![],
                    Return::new(types::Primitive::Float64, Primitive::Float64(42.0)),
                ),
                types::Primitive::Float64,
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
                types::Primitive::Float64,
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
                Block::new(
                    vec![
                        Call::new(function_type, Variable::new("f"), vec![], "x").into(),
                        Load::new(types::Primitive::Float64, Variable::new("x"), "y").into(),
                    ],
                    Return::new(types::Primitive::Float64, Variable::new("y")),
                ),
                types::Primitive::Float64,
            )],
        ));
    }

    #[test]
    fn transform_if_with_return() {
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
                Block::new(
                    vec![If::new(
                        VOID_TYPE.clone(),
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
                types::Primitive::Float64,
            )],
        ));
    }

    #[test]
    fn transform_if_with_branch() {
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
                types::Primitive::Float64,
            )],
        ));
    }

    #[test]
    fn keep_tail_call_in_if_with_branch() {
        let function_type = create_function_type(
            vec![types::Primitive::Float64.into()],
            types::Primitive::Float64,
        );

        insta::assert_snapshot!(format_module(
            &transform_to_cps(
                &Module::new(
                    vec![],
                    vec![FunctionDeclaration::new("f", function_type.clone())],
                    vec![],
                    vec![create_function_definition(
                        "g",
                        vec![],
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
                        types::Primitive::Float64,
                    )],
                ),
                VOID_TYPE.clone()
            )
            .unwrap()
        ));
    }

    #[test]
    fn transform_return_after_if_branch() {
        let function_type = create_function_type(
            vec![types::Primitive::Float64.into()],
            types::Primitive::Float64,
        );

        insta::assert_snapshot!(format_module(
            &transform_to_cps(
                &Module::new(
                    vec![],
                    vec![FunctionDeclaration::new("f", function_type.clone())],
                    vec![],
                    vec![create_function_definition(
                        "g",
                        vec![],
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
                        types::Primitive::Float64,
                    )],
                ),
                VOID_TYPE.clone()
            )
            .unwrap()
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
                types::Primitive::PointerInteger,
            )],
        ));
    }

    #[test]
    fn transform_two_calls_with_if() {
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
                                Branch::new(types::Primitive::PointerInteger, Variable::new("x")),
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
                types::Primitive::PointerInteger,
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
            transform_to_cps(
                &Module::new(
                    vec![],
                    vec![FunctionDeclaration::new("f", function_type.clone())],
                    vec![],
                    vec![create_function_definition(
                        "g",
                        vec![],
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
                        types::Primitive::Float64,
                    )],
                ),
                VOID_TYPE.clone()
            ),
            Ok(Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", cps_function_type.clone())],
                vec![],
                vec![FunctionDefinition::new(
                    "g",
                    vec![
                        Argument::new("_s", STACK_TYPE.clone()),
                        Argument::new(
                            "_k",
                            types::Function::new(
                                vec![STACK_TYPE.clone(), types::Primitive::Float64.into()],
                                VOID_TYPE.clone(),
                                CallingConvention::Tail,
                            )
                        ),
                    ],
                    Block::new(
                        vec![Call::new(
                            cps_function_type,
                            Variable::new("f"),
                            vec![
                                Variable::new("_s").into(),
                                Variable::new("_k").into(),
                                Primitive::Float64(42.0).into()
                            ],
                            "_result",
                        )
                        .into()],
                        Return::new(VOID_TYPE.clone(), Variable::new("_result")),
                    ),
                    VOID_TYPE.clone(),
                    CallingConvention::Tail,
                    Linkage::Internal,
                )],
            ))
        );
    }

    #[test]
    fn transform_tail_call_in_if() {
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
                types::Primitive::Float64,
            )],
        ));
    }

    #[test]
    fn transform_if_with_large_environment() {
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
                types::Primitive::Float64,
            )],
        ));
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
                    Block::new(
                        vec![Call::new(function_type, Variable::new("g"), vec![], "x").into()],
                        Return::new(types::Primitive::Float64, Variable::new("x")),
                    ),
                    types::Primitive::Float64,
                    CallingConvention::Target,
                    Linkage::Internal,
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
                    types::Primitive::Float64,
                    CallingConvention::Target,
                    Linkage::Internal,
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
                    types::Primitive::Float64,
                    CallingConvention::Target,
                    Linkage::Internal,
                )],
            ));
        }
    }
}
