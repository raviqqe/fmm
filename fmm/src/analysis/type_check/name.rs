use super::TypeCheckError;
use crate::ir::*;
use fnv::FnvHashSet;

pub fn check(module: &Module) -> Result<(), TypeCheckError> {
    let variable_names = module
        .variable_declarations()
        .iter()
        .map(|declaration| declaration.name())
        .chain(
            module
                .variable_definitions()
                .iter()
                .map(|definition| definition.name()),
        )
        .collect();
    let function_names = module
        .function_declarations()
        .iter()
        .map(|declaration| declaration.name())
        .chain(
            module
                .function_definitions()
                .iter()
                .map(|definition| definition.name()),
        )
        .collect();

    for (existing_names, names) in [
        (
            &function_names,
            module
                .variable_declarations()
                .iter()
                .map(|declaration| declaration.name())
                .collect::<Vec<_>>(),
        ),
        (
            &function_names,
            module
                .variable_definitions()
                .iter()
                .map(|definition| definition.name())
                .collect(),
        ),
        (
            &variable_names,
            module
                .function_declarations()
                .iter()
                .map(|declaration| declaration.name())
                .collect(),
        ),
        (
            &variable_names,
            module
                .function_definitions()
                .iter()
                .map(|definition| definition.name())
                .collect(),
        ),
    ] {
        check_names(existing_names, &names)?;
    }

    Ok(())
}

fn check_names(existing_names: &FnvHashSet<&str>, names: &[&str]) -> Result<(), TypeCheckError> {
    let mut existing_names = existing_names.clone();

    for name in names {
        if existing_names.contains(name) {
            return Err(TypeCheckError::DuplicateNames(name.to_string()));
        }

        existing_names.insert(name);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types;

    #[test]
    fn check_with_empty_modules() {
        assert_eq!(check(&Module::new(vec![], vec![], vec![], vec![])), Ok(()));
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
            check(&module),
            Err(TypeCheckError::DuplicateNames("f".into()))
        );
    }

    #[test]
    fn check_duplicate_names_in_function_declaration() {
        let type_ = types::Function::new(
            vec![],
            types::Primitive::PointerInteger,
            types::CallingConvention::Source,
        );

        let module = Module::new(
            vec![],
            vec![FunctionDeclaration::new("f", type_.clone())],
            vec![VariableDefinition::new(
                "f",
                Undefined::new(type_.clone()),
                type_.clone(),
                false,
                Linkage::External,
                None,
            )],
            vec![],
        );

        assert_eq!(
            check(&module),
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
            check(&module),
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
            check(&module),
            Err(TypeCheckError::DuplicateNames("f".into()))
        );
    }

    #[test]
    fn allow_duplicate_names_in_variable_declaration_and_definition() {
        let module = Module::new(
            vec![VariableDeclaration::new(
                "x",
                types::Primitive::PointerInteger,
            )],
            vec![],
            vec![VariableDefinition::new(
                "x",
                Primitive::PointerInteger(42),
                types::Primitive::PointerInteger,
                false,
                Linkage::External,
                None,
            )],
            vec![],
        );

        assert_eq!(check(&module), Ok(()));
    }

    #[test]
    fn allow_duplicate_names_in_function_declaration_and_definition() {
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

        assert_eq!(check(&module), Ok(()));
    }
}
