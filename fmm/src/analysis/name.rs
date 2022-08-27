use super::TypeCheckError;
use crate::ir::*;
use fnv::FnvHashSet;

pub fn check(module: &Module) -> Result<(), TypeCheckError> {
    let variable_definition_names = module
        .variable_definitions()
        .iter()
        .map(|definition| definition.name())
        .collect::<FnvHashSet<_>>();
    let function_definition_names = module
        .function_definitions()
        .iter()
        .map(|definition| definition.name())
        .collect::<FnvHashSet<_>>();

    let mut names = FnvHashSet::default();
    let mut declaration_names = FnvHashSet::default();

    for definition in module.variable_definitions() {
        check_name(definition.name(), &mut names)?;
    }

    for declaration in module.variable_declarations() {
        if !variable_definition_names.contains(declaration.name()) {
            check_name(declaration.name(), &mut names)?;
        }

        check_name(declaration.name(), &mut declaration_names)?;
    }

    for definition in module.function_definitions() {
        check_name(definition.name(), &mut names)?;
    }

    for declaration in module.function_declarations() {
        if !function_definition_names.contains(declaration.name()) {
            check_name(declaration.name(), &mut names)?;
        }

        check_name(declaration.name(), &mut declaration_names)?;
    }

    for definition in module.function_definitions() {
        check_block(definition.body(), &mut names)?;
    }

    Ok(())
}

fn check_block<'a>(
    block: &'a Block,
    variables: &mut FnvHashSet<&'a str>,
) -> Result<(), TypeCheckError> {
    for instruction in block.instructions() {
        check_instruction(instruction, variables)?;
    }

    Ok(())
}

fn check_instruction<'a>(
    instruction: &'a Instruction,
    names: &mut FnvHashSet<&'a str>,
) -> Result<(), TypeCheckError> {
    match instruction {
        Instruction::If(if_) => {
            check_block(if_.then(), names)?;
            check_block(if_.else_(), names)?;
        }
        Instruction::AllocateHeap(_)
        | Instruction::AllocateStack(_)
        | Instruction::AtomicLoad(_)
        | Instruction::AtomicOperation(_)
        | Instruction::AtomicStore(_)
        | Instruction::Call(_)
        | Instruction::CompareAndSwap(_)
        | Instruction::DeconstructRecord(_)
        | Instruction::DeconstructUnion(_)
        | Instruction::Fence(_)
        | Instruction::FreeHeap(_)
        | Instruction::Load(_)
        | Instruction::ReallocateHeap(_)
        | Instruction::Store(_) => {}
    }

    if let Some((name, _)) = instruction.value() {
        check_name(name, names)?
    }

    Ok(())
}

fn check_name<'a>(name: &'a str, names: &mut FnvHashSet<&'a str>) -> Result<(), TypeCheckError> {
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

    #[test]
    fn allow_duplicate_names_in_variable_and_function_declarations() {
        let module = Module::new(
            vec![VariableDeclaration::new(
                "f",
                types::Primitive::PointerInteger,
            )],
            vec![FunctionDeclaration::new(
                "f",
                types::Function::new(
                    vec![],
                    types::Primitive::PointerInteger,
                    types::CallingConvention::Source,
                ),
            )],
            vec![],
            vec![],
        );

        assert_eq!(
            check(&module),
            Err(TypeCheckError::DuplicateNames("f".into()))
        );
    }

    #[test]
    fn check_duplicate_names_in_instructions() {
        let module = Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![
                        AllocateStack::new(types::Primitive::PointerInteger, "x").into(),
                        AllocateStack::new(types::Primitive::PointerInteger, "x").into(),
                    ],
                    TerminalInstruction::Unreachable,
                ),
                Default::default(),
            )],
        );

        assert_eq!(
            check(&module),
            Err(TypeCheckError::DuplicateNames("x".into()))
        );
    }

    #[test]
    fn check_duplicate_names_in_instructions_in_different_function_definitions() {
        let module = Module::new(
            vec![],
            vec![],
            vec![],
            vec![
                FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![AllocateStack::new(types::Primitive::PointerInteger, "x").into()],
                        TerminalInstruction::Unreachable,
                    ),
                    Default::default(),
                ),
                FunctionDefinition::new(
                    "g",
                    vec![],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![AllocateStack::new(types::Primitive::PointerInteger, "x").into()],
                        TerminalInstruction::Unreachable,
                    ),
                    Default::default(),
                ),
            ],
        );

        assert_eq!(check(&module), Ok(()));
    }
}
