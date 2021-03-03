mod cps_transformer;
mod error;
mod free_variables;
mod stack;
mod target_functions;

use crate::ir::*;
use crate::types::Type;
use cps_transformer::*;
use error::CpsTransformationError;

pub fn transform_to_cps(
    module: &Module,
    result_type: impl Into<Type>,
) -> Result<Module, CpsTransformationError> {
    CpsTransformer::new(result_type).transform(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::check_types;
    use crate::types::{self, CallingConvention, Type};
    use once_cell::sync::Lazy;

    static VOID_TYPE: Lazy<Type> = Lazy::new(|| types::Record::new(vec![]).into());

    fn create_function_type(arguments: Vec<Type>, result: impl Into<Type>) -> types::Function {
        types::Function::new(arguments, result, CallingConvention::Source)
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
            false,
        )
    }

    fn test_transformation(module: &Module) {
        check_types(&transform_to_cps(module, VOID_TYPE.clone()).unwrap()).unwrap();
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
    #[should_panic]
    fn transform_call_in_function_of_target_calling_convention() {
        let function_type = create_function_type(
            vec![types::Primitive::Float64.into()],
            types::Primitive::Float64,
        );

        test_transformation(&Module::new(
            vec![],
            vec![FunctionDeclaration::new("f", function_type.clone())],
            vec![],
            vec![FunctionDefinition::new(
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
                CallingConvention::Target,
                false,
            )],
        ));
    }

    #[test]
    fn transform_instruction_after_call() {
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
                vec![Argument::new("x", types::Primitive::Float64)],
                Block::new(
                    vec![
                        Call::new(
                            function_type,
                            Variable::new("f"),
                            vec![Primitive::Float64(42.0).into()],
                            "y",
                        )
                        .into(),
                        ArithmeticOperation::new(
                            types::Primitive::Float64,
                            ArithmeticOperator::Add,
                            Variable::new("x"),
                            Variable::new("y"),
                            "z",
                        )
                        .into(),
                    ],
                    Return::new(types::Primitive::Float64, Variable::new("z")),
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
}
