use super::free_variable_collector;
use crate::{
    build::NameGenerator,
    ir::*,
    types::{self, Type, VOID_TYPE},
};
use std::collections::HashMap;

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
    if definition.calling_convention() == types::CallingConvention::Source {
        FunctionDefinition::new(
            definition.name(),
            definition.arguments().to_vec(),
            transform_block(
                context,
                definition.body(),
                definition.result_type(),
                &definition
                    .arguments()
                    .iter()
                    .map(|argument| (argument.name().into(), argument.type_().clone()))
                    .collect(),
            ),
            definition.result_type().clone(),
            definition.calling_convention(),
            definition.linkage(),
        )
    } else {
        definition.clone()
    }
}

fn transform_block(
    context: &mut Context,
    block: &Block,
    result_type: &Type,
    local_variables: &HashMap<String, Type>,
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

fn transform_instructions(
    context: &mut Context,
    instructions: &[Instruction],
    terminal_instruction: &TerminalInstruction,
    result_type: &Type,
    local_variables: &HashMap<String, Type>,
) -> (Vec<Instruction>, TerminalInstruction) {
    match instructions {
        [] => (vec![], terminal_instruction.clone()),
        [instruction, ..] => {
            let instructions = &instructions[1..];

            if let Instruction::If(if_) = instruction {
                let environment = get_continuation_environment(
                    instructions,
                    terminal_instruction,
                    &local_variables
                        .clone()
                        .into_iter()
                        .chain(vec![(if_.name().into(), if_.type_().clone())])
                        .collect(),
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
                        VOID_TYPE.clone(),
                        if_.condition().clone(),
                        transform_if_block(
                            context,
                            if_.name(),
                            if_.then(),
                            result_type,
                            local_variables,
                            &continuation,
                            &environment,
                        ),
                        transform_if_block(
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
                    &local_variables
                        .clone()
                        .into_iter()
                        .chain(instruction.name().and_then(|name| {
                            instruction.result_type().map(|type_| (name.into(), type_))
                        }))
                        .collect(),
                );

                (
                    vec![instruction.clone()]
                        .into_iter()
                        .chain(instructions)
                        .collect(),
                    terminal_instruction,
                )
            }
        }
    }
}

fn transform_if_block(
    context: &mut Context,
    if_name: &str,
    block: &Block,
    result_type: &Type,
    local_variables: &HashMap<String, Type>,
    continuation: &Expression,
    environment: &[(String, Type)],
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
                    .chain(vec![Call::new(
                        types::Function::new(
                            environment.iter().map(|(_, type_)| type_.clone()).collect(),
                            result_type.clone(),
                            types::CallingConvention::Source,
                        ),
                        continuation.clone(),
                        environment
                            .iter()
                            .map(|(name, _)| {
                                if name == if_name {
                                    branch.expression().clone()
                                } else {
                                    Variable::new(name).into()
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
    environment: &[(String, Type)],
) -> Variable {
    let name = context.name_generator.generate();
    let block = transform_block(
        context,
        &Block::new(instructions.to_vec(), terminal_instruction.clone()),
        result_type,
        &environment.iter().cloned().collect(),
    );

    context.function_definitions.push(FunctionDefinition::new(
        &name,
        environment
            .iter()
            .map(|(name, type_)| Argument::new(name, type_.clone()))
            .collect(),
        block,
        result_type.clone(),
        types::CallingConvention::Source,
        Linkage::Internal,
    ));

    Variable::new(name)
}

fn get_continuation_environment(
    instructions: &[Instruction],
    terminal_instruction: &TerminalInstruction,
    local_variables: &HashMap<String, Type>,
) -> Vec<(String, Type)> {
    free_variable_collector::collect(instructions, terminal_instruction)
        .iter()
        .flat_map(|name| {
            local_variables
                .get(name)
                .map(|type_| (name.clone(), type_.clone()))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{analysis::check_types, types::VOID_TYPE};

    fn flatten_module(module: &Module) {
        check_types(&flatten(module)).unwrap();
    }

    fn create_function_type(arguments: Vec<Type>, result: impl Into<Type>) -> types::Function {
        types::Function::new(arguments, result, types::CallingConvention::Source)
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
            types::CallingConvention::Source,
            Linkage::Internal,
        )
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
                Block::new(
                    vec![],
                    Return::new(types::Primitive::Float64, Primitive::Float64(42.0)),
                ),
                types::Primitive::Float64,
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
}
