use super::TypeCheckError;
use crate::ir::*;
use fnv::{FnvHashMap, FnvHashSet};

pub fn check_names(module: &Module) -> Result<(), TypeCheckError> {
    let mut names = HashSet::new();

    for name in module
        .variable_declarations()
        .iter()
        .map(|declaration| declaration.name())
        .chain(
            module
                .variable_definitions()
                .iter()
                .map(|declaration| declaration.name()),
        )
        .chain(
            module
                .function_declarations()
                .iter()
                .map(|declaration| declaration.name()),
        )
        .chain(
            module
                .function_definitions()
                .iter()
                .map(|definition| definition.name()),
        )
    {
        if names.contains(name) {
            return Err(TypeCheckError::DuplicateNames(name.into()));
        }

        names.insert(name);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types;

    #[test]
    fn check_with_empty_modules() {
        assert_eq!(
            check_names(&Module::new(vec![], vec![], vec![], vec![])),
            Ok(())
        );
    }

    #[test]
    fn check_duplicate_names_in_function_definition() {
        let module = Module::new(
            vec![],
            vec![],
            vec![],
            vec![
                FunctionDefinition::new(
                    "f",
                    vec![],
                    Block::new(vec![], TerminalInstruction::Unreachable),
                    types::Primitive::PointerInteger,
                    types::CallingConvention::Source,
                    Linkage::External,
                ),
                FunctionDefinition::new(
                    "f",
                    vec![],
                    Block::new(vec![], TerminalInstruction::Unreachable),
                    types::Primitive::PointerInteger,
                    types::CallingConvention::Source,
                    Linkage::External,
                ),
            ],
        );

        assert_eq!(
            check_names(&module),
            Err(TypeCheckError::DuplicateNames("f".into()))
        );
    }

    #[test]
    fn check_duplicate_names_in_function_declaration() {
        let module = Module::new(
            vec![],
            vec![FunctionDeclaration::new(
                "f",
                types::Function::new(
                    vec![],
                    types::Primitive::PointerInteger,
                    types::CallingConvention::Source,
                ),
            )],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![],
                Block::new(vec![], TerminalInstruction::Unreachable),
                types::Primitive::PointerInteger,
                types::CallingConvention::Source,
                Linkage::External,
            )],
        );

        assert_eq!(
            check_names(&module),
            Err(TypeCheckError::DuplicateNames("f".into()))
        );
    }

    #[test]
    fn check_duplicate_names_in_variable_definition() {
        let module = Module::new(
            vec![],
            vec![],
            vec![VariableDefinition::new(
                "f",
                Primitive::PointerInteger(42),
                types::Primitive::PointerInteger,
                false,
                Linkage::External,
                None,
            )],
            vec![FunctionDefinition::new(
                "f",
                vec![],
                Block::new(vec![], TerminalInstruction::Unreachable),
                types::Primitive::PointerInteger,
                types::CallingConvention::Source,
                Linkage::External,
            )],
        );

        assert_eq!(
            check_names(&module),
            Err(TypeCheckError::DuplicateNames("f".into()))
        );
    }

    #[test]
    fn check_duplicate_names_in_variable_declaration() {
        let module = Module::new(
            vec![VariableDeclaration::new(
                "f",
                types::Primitive::PointerInteger,
            )],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![],
                Block::new(vec![], TerminalInstruction::Unreachable),
                types::Primitive::PointerInteger,
                types::CallingConvention::Source,
                Linkage::External,
            )],
        );

        assert_eq!(
            check_names(&module),
            Err(TypeCheckError::DuplicateNames("f".into()))
        );
    }
}
