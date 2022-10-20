mod error;

pub use self::error::NameError;
use crate::ir::*;
use fnv::FnvHashSet;

pub fn check(module: &Module) -> Result<(), NameError> {
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

    let mut global_names = FnvHashSet::default();
    let mut declaration_names = FnvHashSet::default();

    for definition in module.variable_definitions() {
        check_name(definition.name(), &mut global_names)?;
    }

    for declaration in module.variable_declarations() {
        if !variable_definition_names.contains(declaration.name()) {
            check_name(declaration.name(), &mut global_names)?;
        }

        check_name(declaration.name(), &mut declaration_names)?;
    }

    for definition in module.function_definitions() {
        check_name(definition.name(), &mut global_names)?;
    }

    for declaration in module.function_declarations() {
        if !function_definition_names.contains(declaration.name()) {
            check_name(declaration.name(), &mut global_names)?;
        }

        check_name(declaration.name(), &mut declaration_names)?;
    }

    for definition in module.function_definitions() {
        let mut local_names = FnvHashSet::default();

        check_block(definition.body(), &mut local_names, &global_names)?;
    }

    Ok(())
}

fn check_block<'a>(
    block: &'a Block,
    local_names: &mut FnvHashSet<&'a str>,
    global_names: &FnvHashSet<&'a str>,
) -> Result<(), NameError> {
    for instruction in block.instructions() {
        check_instruction(instruction, local_names, global_names)?;
    }

    Ok(())
}

fn check_instruction<'a>(
    instruction: &'a Instruction,
    local_names: &mut FnvHashSet<&'a str>,
    global_names: &FnvHashSet<&'a str>,
) -> Result<(), NameError> {
    match instruction {
        Instruction::If(if_) => {
            check_block(if_.then(), local_names, global_names)?;
            check_block(if_.else_(), local_names, global_names)?;
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
        | Instruction::MemoryCopy(_)
        | Instruction::ReallocateHeap(_)
        | Instruction::Store(_) => {}
    }

    if let Some((name, _)) = instruction.value() {
        if !name.is_empty() {
            check_name(name, local_names)?;
        }

        if global_names.contains(name) {
            return Err(NameError::DuplicateNames(name.into()));
        }
    }

    Ok(())
}

fn check_name<'a>(name: &'a str, names: &mut FnvHashSet<&'a str>) -> Result<(), NameError> {
    if names.contains(name) {
        return Err(NameError::DuplicateNames(name.into()));
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

    mod global {
        use super::*;

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

            assert_eq!(check(&module), Err(NameError::DuplicateNames("f".into())));
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

            assert_eq!(check(&module), Err(NameError::DuplicateNames("f".into())));
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

            assert_eq!(check(&module), Err(NameError::DuplicateNames("f".into())));
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

            assert_eq!(check(&module), Err(NameError::DuplicateNames("f".into())));
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

            assert_eq!(check(&module), Err(NameError::DuplicateNames("f".into())));
        }
    }

    mod local {
        use super::*;

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

            assert_eq!(check(&module), Err(NameError::DuplicateNames("x".into())));
        }

        #[test]
        fn check_duplicate_names_in_argument_and_instruction() {
            let module = Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![Argument::new("x", types::Primitive::PointerInteger)],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![AllocateStack::new(types::Primitive::PointerInteger, "x").into()],
                        TerminalInstruction::Unreachable,
                    ),
                    Default::default(),
                )],
            );

            assert_eq!(check(&module), Err(NameError::DuplicateNames("x".into())));
        }

        #[test]
        fn check_duplicate_names_in_instruction_and_function_definition() {
            let module = Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![AllocateStack::new(types::Primitive::PointerInteger, "f").into()],
                        TerminalInstruction::Unreachable,
                    ),
                    Default::default(),
                )],
            );

            assert_eq!(check(&module), Err(NameError::DuplicateNames("f".into())));
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

        #[test]
        fn check_duplicate_empty_names() {
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
                            AllocateStack::new(types::Primitive::PointerInteger, "").into(),
                            AllocateStack::new(types::Primitive::PointerInteger, "").into(),
                        ],
                        TerminalInstruction::Unreachable,
                    ),
                    Default::default(),
                )],
            );

            assert_eq!(check(&module), Ok(()));
        }
    }
}
