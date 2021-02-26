mod cps_transformer;
mod free_variables;
mod stack;

use crate::ir::*;
use crate::types::Type;
use cps_transformer::*;

pub fn transform_to_cps(module: &Module, result_type: impl Into<Type>) -> Module {
    CpsTransformer::new(result_type).transform(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::check_types;
    use crate::types::{self, CallingConvention, Type};

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
        check_types(&transform_to_cps(module, types::Record::new(vec![]))).unwrap();
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
                        function_type.clone(),
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
                            function_type.clone(),
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
}
