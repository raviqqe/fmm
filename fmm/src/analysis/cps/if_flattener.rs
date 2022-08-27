use super::free_variable_collector;
use crate::{
    analysis::convert_expressions_in_terminal_instruction,
    build::NameGenerator,
    ir::*,
    types::{self, void_type, Type},
};
use fnv::FnvHashMap;

struct Context {
    function_definitions: Vec<FunctionDefinition>,
    name_generator: NameGenerator,
}

// TODO Consider integrating this logic deeply with CPS transformation to omit
// extra CPS stack manipulation.
pub fn flatten(module: &Module) -> Module {
    let mut context = Context {
        function_definitions: vec![],
        name_generator: NameGenerator::new("_if_"),
    };

    Module::new(
        module.variable_declarations().to_vec(),
        module.function_declarations().to_vec(),
        module.variable_definitions().to_vec(),
        module
            .function_definitions()
            .iter()
            .map(|definition| transform_function_definition(&mut context, definition))
            .collect::<Vec<_>>()
            .into_iter()
            .chain(context.function_definitions)
            .collect::<Vec<_>>(),
    )
}

fn transform_function_definition(
    context: &mut Context,
    definition: &FunctionDefinition,
) -> FunctionDefinition {
    if definition.type_().calling_convention() == types::CallingConvention::Source {
        FunctionDefinition::new(
            definition.name(),
            definition.arguments().to_vec(),
            definition.result_type().clone(),
            transform_block(
                context,
                definition.body(),
                definition.result_type(),
                &mut definition
                    .arguments()
                    .iter()
                    .map(|argument| (argument.name(), argument.type_().clone()))
                    .collect(),
            ),
            definition.options().clone(),
        )
    } else {
        definition.clone()
    }
}

fn transform_block(
    context: &mut Context,
    block: &Block,
    result_type: &Type,
    local_variables: &mut FnvHashMap<&str, Type>,
) -> Block {
    let (instructions, terminal_instruction) = transform_instructions(
        context,
        block.instructions(),
        block.terminal_instruction(),
        result_type,
        local_variables,
    );

    Block::new(instructions, terminal_instruction)
}

fn transform_instructions<'a>(
    context: &mut Context,
    instructions: &'a [Instruction],
    terminal_instruction: &TerminalInstruction,
    result_type: &Type,
    local_variables: &mut FnvHashMap<&'a str, Type>,
) -> (Vec<Instruction>, TerminalInstruction) {
    match instructions {
        [] => (vec![], terminal_instruction.clone()),
        [instruction] => {
            if let Instruction::If(if_) = instruction {
                (
                    vec![If::new(
                        void_type(),
                        if_.condition().clone(),
                        transform_if_block_without_continuation(
                            context,
                            if_.name(),
                            if_.then(),
                            result_type,
                            local_variables,
                            terminal_instruction,
                        ),
                        transform_if_block_without_continuation(
                            context,
                            if_.name(),
                            if_.else_(),
                            result_type,
                            local_variables,
                            terminal_instruction,
                        ),
                        "",
                    )
                    .into()],
                    TerminalInstruction::Unreachable,
                )
            } else {
                (vec![instruction.clone()], terminal_instruction.clone())
            }
        }
        [instruction, ..] => {
            let instructions = &instructions[1..];

            if let Some((name, type_)) = instruction.value() {
                local_variables.insert(name, type_);
            }

            if let Instruction::If(if_) = instruction {
                let environment = get_continuation_environment(
                    instructions,
                    terminal_instruction,
                    local_variables,
                );
                let continuation = create_continuation(
                    context,
                    instructions,
                    terminal_instruction,
                    result_type,
                    &environment,
                )
                .into();

                (
                    vec![If::new(
                        void_type(),
                        if_.condition().clone(),
                        transform_if_block_with_continuation(
                            context,
                            if_.name(),
                            if_.then(),
                            result_type,
                            local_variables,
                            &continuation,
                            &environment,
                        ),
                        transform_if_block_with_continuation(
                            context,
                            if_.name(),
                            if_.else_(),
                            result_type,
                            local_variables,
                            &continuation,
                            &environment,
                        ),
                        "",
                    )
                    .into()],
                    TerminalInstruction::Unreachable,
                )
            } else {
                let (instructions, terminal_instruction) = transform_instructions(
                    context,
                    instructions,
                    terminal_instruction,
                    result_type,
                    local_variables,
                );

                (
                    [instruction.clone()]
                        .into_iter()
                        .chain(instructions)
                        .collect(),
                    terminal_instruction,
                )
            }
        }
    }
}

fn transform_if_block_without_continuation(
    context: &mut Context,
    if_name: &str,
    block: &Block,
    result_type: &Type,
    local_variables: &mut FnvHashMap<&str, Type>,
    terminal_instruction: &TerminalInstruction,
) -> Block {
    transform_block(
        context,
        &Block::new(
            block.instructions().to_vec(),
            match (block.terminal_instruction(), terminal_instruction) {
                (_, TerminalInstruction::Branch(_)) => unreachable!(),
                (TerminalInstruction::Return(return_), _) => return_.clone().into(),
                (TerminalInstruction::Unreachable, _) => TerminalInstruction::Unreachable,
                (TerminalInstruction::Branch(_), TerminalInstruction::Unreachable) => {
                    TerminalInstruction::Unreachable
                }
                (TerminalInstruction::Branch(branch), return_) => {
                    convert_expressions_in_terminal_instruction(return_, &|expression| {
                        if expression == &Variable::new(if_name).into() {
                            branch.expression().clone()
                        } else {
                            expression.clone()
                        }
                    })
                }
            },
        ),
        result_type,
        local_variables,
    )
}

fn transform_if_block_with_continuation(
    context: &mut Context,
    if_name: &str,
    block: &Block,
    result_type: &Type,
    local_variables: &mut FnvHashMap<&str, Type>,
    continuation: &Expression,
    environment: &[(&str, Type)],
) -> Block {
    if let TerminalInstruction::Branch(branch) = block.terminal_instruction() {
        let result_name = context.name_generator.generate();

        transform_block(
            context,
            &Block::new(
                block
                    .instructions()
                    .iter()
                    .cloned()
                    .chain([Call::new(
                        types::Function::new(
                            environment.iter().map(|(_, type_)| type_.clone()).collect(),
                            result_type.clone(),
                            types::CallingConvention::Source,
                        ),
                        continuation.clone(),
                        environment
                            .iter()
                            .map(|(name, _)| {
                                if name == &if_name {
                                    branch.expression().clone()
                                } else {
                                    Variable::new(*name).into()
                                }
                            })
                            .collect(),
                        &result_name,
                    )
                    .into()])
                    .collect(),
                Return::new(result_type.clone(), Variable::new(result_name)),
            ),
            result_type,
            local_variables,
        )
    } else {
        transform_block(context, block, result_type, local_variables)
    }
}

fn create_continuation(
    context: &mut Context,
    instructions: &[Instruction],
    terminal_instruction: &TerminalInstruction,
    result_type: &Type,
    environment: &[(&str, Type)],
) -> Variable {
    let name = context.name_generator.generate();
    let block = transform_block(
        context,
        &Block::new(instructions.to_vec(), terminal_instruction.clone()),
        result_type,
        &mut environment.iter().cloned().collect(),
    );

    context.function_definitions.push(FunctionDefinition::new(
        &name,
        environment
            .iter()
            .map(|(name, type_)| Argument::new(*name, type_.clone()))
            .collect(),
        result_type.clone(),
        block,
        FunctionDefinitionOptions::new()
            .set_address_named(false)
            .set_calling_convention(types::CallingConvention::Source)
            .set_linkage(Linkage::Internal),
    ));

    Variable::new(name)
}

fn get_continuation_environment<'a>(
    instructions: &'a [Instruction],
    terminal_instruction: &'a TerminalInstruction,
    local_variables: &FnvHashMap<&str, Type>,
) -> Vec<(&'a str, Type)> {
    free_variable_collector::collect(instructions, terminal_instruction)
        .iter()
        .flat_map(|&name| local_variables.get(name).map(|type_| (name, type_.clone())))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{analysis::check_types, types::void_type};

    fn flatten_module(module: &Module) {
        let flattened = flatten(module);

        check_types(&flattened).unwrap();

        assert_eq!(flattened, flatten(module));
    }

    fn create_function_type(arguments: Vec<Type>, result: impl Into<Type>) -> types::Function {
        types::Function::new(arguments, result, types::CallingConvention::Source)
    }

    fn create_function_definition(
        name: impl Into<String>,
        arguments: Vec<Argument>,
        result_type: impl Into<Type>,
        body: Block,
    ) -> FunctionDefinition {
        FunctionDefinition::new(name, arguments, result_type, body, Default::default())
    }

    #[test]
    fn transform_empty_module() {
        flatten_module(&Module::new(vec![], vec![], vec![], vec![]));
    }

    #[test]
    fn transform_function_definition() {
        flatten_module(&Module::new(
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
    fn flatten_if_with_return() {
        let function_type = create_function_type(
            vec![types::Primitive::Float64.into()],
            types::Primitive::Float64,
        );

        flatten_module(&Module::new(
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
    fn flatten_if_with_branch() {
        let function_type = create_function_type(
            vec![types::Primitive::Float64.into()],
            types::Primitive::Float64,
        );

        flatten_module(&Module::new(
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
    fn flatten_if_with_large_environment() {
        let function_type = create_function_type(
            vec![types::Primitive::Float64.into()],
            types::Primitive::Float64,
        );

        flatten_module(&Module::new(
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
}
