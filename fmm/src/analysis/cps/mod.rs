mod cps_transformer;

use crate::ir::*;
use cps_transformer::*;

pub fn transform_to_cps(module: &Module) -> Module {
    CpsTransformer::new().transform(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::{check_types, TypeCheckError};
    use crate::types::{self, CallingConvention, Type};

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
            CallingConvention::Direct,
            false,
        )
    }

    #[test]
    fn transform_empty_module() -> Result<(), TypeCheckError> {
        check_types(&transform_to_cps(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![],
        )))
    }

    #[test]
    fn transform_function_definition() -> Result<(), TypeCheckError> {
        check_types(&transform_to_cps(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![],
                    Return::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(42),
                    ),
                ),
                types::Primitive::PointerInteger,
            )],
        )))
    }
}
