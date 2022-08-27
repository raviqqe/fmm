use super::TypeCheckError;
use crate::ir::*;
use fnv::FnvHashSet;

pub fn check(module: &Module) -> Result<(), TypeCheckError> {
    let variable_declaration_names = module
        .variable_declarations()
        .iter()
        .map(|declaration| declaration.name())
        .collect();
    let function_declaration_names = module
        .function_declarations()
        .iter()
        .map(|declaration| declaration.name())
        .collect();

    let mut names = FnvHashSet::default();

    for declaration in module.variable_declarations() {
        check_name(declaration.name(), &mut names);
    }

    for definition in module.variable_definitions() {
        check_name(definition.name(), &mut names);
    }

    for declaration in module.function_declarations() {
        check_name(declaration.name(), &mut names);
    }

    for definition in module.function_definitions() {
        check_name(definition.name(), &mut names);
    }

    Ok(())
}

fn check_name(name: &str, names: &mut FnvHashSet<&str>) -> Result<(), TypeCheckError> {
    if names.contains(name) {
        return Err(TypeCheckError::DuplicateNames(name.into()));
    }

    names.insert(name);

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
                    types::Primitive::PointerInteger,
                    Block::new(vec![], TerminalInstruction::Unreachable),
                    Default::default(),
                ),
                FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::PointerInteger,
                    Block::new(vec![], TerminalInstruction::Unreachable),
                    Default::default(),
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
                type_,
                Default::default(),
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
                Default::default(),
            )],
            vec![FunctionDefinition::new(
                "f",
                vec![],
                types::Primitive::PointerInteger,
                Block::new(vec![], TerminalInstruction::Unreachable),
                Default::default(),
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
                types::Primitive::PointerInteger,
                Block::new(vec![], TerminalInstruction::Unreachable),
                Default::default(),
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
                Default::default(),
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
                types::Primitive::PointerInteger,
                Block::new(vec![], TerminalInstruction::Unreachable),
                Default::default(),
            )],
        );

        assert_eq!(check(&module), Ok(()));
    }
}
