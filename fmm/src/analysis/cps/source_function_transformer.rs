use super::{
    context::CpsContext,
    error::CpsTransformationError,
    free_variable_collector,
    stack::{pop_from_stack, push_to_stack, STACK_TYPE},
};
use crate::{
    analysis::cps::continuation_type_compiler,
    build::{self, BuildError, InstructionBuilder},
    ir::*,
    types::{CallingConvention, Type},
};

const STACK_ARGUMENT_NAME: &str = "_s";
const CONTINUATION_ARGUMENT_NAME: &str = "_k";
const RESULT_NAME: &str = "_result";

struct Context<'a> {
    pub cps: &'a CpsContext,
    pub function_definitions: Vec<FunctionDefinition>,
}

pub fn transform(context: &CpsContext, module: &Module) -> Result<Module, CpsTransformationError> {
    let mut context = Context {
        cps: context,
        function_definitions: vec![],
    };

    Ok(Module::new(
        module.variable_declarations().to_vec(),
        module.function_declarations().to_vec(),
        module.variable_definitions().to_vec(),
        module
            .function_definitions()
            .iter()
            .map(|definition| transform_function_definition(&mut context, definition))
            .collect::<Vec<_>>()
            .into_iter()
            .chain(context.function_definitions.drain(..).map(Ok))
            .collect::<Result<_, _>>()?,
    ))
}

fn transform_function_definition(
    context: &mut Context,
    definition: &FunctionDefinition,
) -> Result<FunctionDefinition, CpsTransformationError> {
    Ok(
        if definition.calling_convention() == CallingConvention::Source {
            let continuation_type = continuation_type_compiler::compile(
                definition.result_type(),
                context.cps.result_type(),
            );

            FunctionDefinition::new(
                definition.name(),
                [
                    Argument::new(STACK_ARGUMENT_NAME, STACK_TYPE.clone()),
                    Argument::new(CONTINUATION_ARGUMENT_NAME, continuation_type.clone()),
                ].into_iter()
                .chain(definition.arguments().iter().cloned())
                .collect(),
                transform_block(
                    context,
                    definition.body(),
                    &definition
                        .arguments()
                        .iter()
                        .map(|argument| (argument.name().into(), argument.type_().clone()))
                        .chain([(CONTINUATION_ARGUMENT_NAME.into(), continuation_type.into())])
                        .collect(),
                )?,
                context.cps.result_type().clone(),
                CallingConvention::Tail,
                definition.linkage(),
            )
        } else {
            definition.clone()
        },
    )
}

fn transform_block(
    context: &mut Context,
    block: &Block,
    local_variables: &hamt::Map<String, Type>,
) -> Result<Block, BuildError> {
    let (instructions, terminal_instruction) = transform_instructions(
        context,
        block.instructions(),
        block.terminal_instruction(),
        local_variables,
    )?;

    Ok(Block::new(instructions, terminal_instruction))
}

fn transform_instructions(
    context: &mut Context,
    instructions: &[Instruction],
    terminal_instruction: &TerminalInstruction,
    local_variables: &hamt::Map<String, Type>,
) -> Result<(Vec<Instruction>, TerminalInstruction), BuildError> {
    Ok(match instructions {
        [] => match terminal_instruction {
            TerminalInstruction::Branch(_) => unreachable!(),
            TerminalInstruction::Return(return_) => (
                vec![Call::new(
                    continuation_type_compiler::compile(return_.type_(), context.cps.result_type()),
                    Variable::new(CONTINUATION_ARGUMENT_NAME),
                    vec![
                        Variable::new(STACK_ARGUMENT_NAME).into(),
                        return_.expression().clone(),
                    ],
                    RESULT_NAME,
                )
                .into()],
                Return::new(
                    context.cps.result_type().clone(),
                    Variable::new(RESULT_NAME),
                )
                .into(),
            ),
            TerminalInstruction::Unreachable => (vec![], TerminalInstruction::Unreachable),
        },
        [instruction, ..] => {
            let instructions = &instructions[1..];

            if let Instruction::Call(call) = instruction {
                if call.type_().calling_convention() == CallingConvention::Source {
                    let is_tail_call = instructions.is_empty()
                        && if let TerminalInstruction::Return(return_) = terminal_instruction {
                            return_.expression() == &Variable::new(call.name()).into()
                        } else {
                            false
                        };

                    let environment = get_continuation_environment(
                        instructions,
                        terminal_instruction,
                        local_variables,
                    );
                    let continuation = if is_tail_call {
                        Variable::new(CONTINUATION_ARGUMENT_NAME).into()
                    } else {
                        create_continuation(
                            context,
                            call,
                            instructions,
                            terminal_instruction,
                            &environment,
                        )?
                    };

                    let builder = InstructionBuilder::new(context.cps.name_generator());

                    if !is_tail_call {
                        push_to_stack(
                            &builder,
                            build::variable(STACK_ARGUMENT_NAME, STACK_TYPE.clone()),
                            get_environment_record(&environment),
                        )?;
                    }

                    return Ok((
                        builder
                            .into_instructions()
                            .into_iter()
                            .chain([Call::new(
                                call.type_().clone(),
                                call.function().clone(),
                                [Variable::new(STACK_ARGUMENT_NAME).into(), continuation].into_iter()
                                    .chain(call.arguments().iter().cloned())
                                    .collect(),
                                RESULT_NAME,
                            )
                            .into()])
                            .collect(),
                        Return::new(
                            context.cps.result_type().clone(),
                            Variable::new(RESULT_NAME),
                        )
                        .into(),
                    ));
                }
            } else if let Instruction::If(if_) = instruction {
                // If instruction is always at tail due to if flattening.
                return Ok((
                    vec![If::new(
                        if_.type_().clone(),
                        if_.condition().clone(),
                        transform_block(context, if_.then(), local_variables)?,
                        transform_block(context, if_.else_(), local_variables)?,
                        context.cps.name_generator().borrow_mut().generate(),
                    )
                    .into()],
                    TerminalInstruction::Unreachable,
                ));
            }

            let (instructions, terminal_instruction) = transform_instructions(
                context,
                instructions,
                terminal_instruction,
                &if let (Some(name), Some(type_)) = (instruction.name(), instruction.result_type())
                {
                    local_variables.insert(name.into(), type_)
                } else {
                    local_variables.clone()
                },
            )?;

            (
                [instruction.clone()].into_iter()
                    .chain(instructions)
                    .collect(),
                terminal_instruction,
            )
        }
    })
}

fn get_environment_record(environment: &[(String, Type)]) -> Record {
    build::record(
        environment
            .iter()
            .map(|(name, type_)| build::variable(name.clone(), type_.clone()))
            .collect(),
    )
}

fn create_continuation(
    context: &mut Context,
    call: &Call,
    instructions: &[Instruction],
    terminal_instruction: &TerminalInstruction,
    environment: &[(String, Type)],
) -> Result<Expression, BuildError> {
    let name = context.cps.name_generator().borrow_mut().generate();
    let block = transform_block(
        context,
        &Block::new(instructions.to_vec(), terminal_instruction.clone()),
        &environment
            .iter()
            .cloned()
            .chain([(call.name().into(), call.type_().result().clone())])
            .collect(),
    )?;

    context.function_definitions.push(FunctionDefinition::new(
        &name,
        vec![
            Argument::new(STACK_ARGUMENT_NAME, STACK_TYPE.clone()),
            Argument::new(call.name(), call.type_().result().clone()),
        ],
        Block::new(
            {
                let builder = InstructionBuilder::new(context.cps.name_generator());

                let environment_record_type = get_environment_record(environment).type_().clone();
                let environment_record = pop_from_stack(
                    &builder,
                    build::variable(STACK_ARGUMENT_NAME, STACK_TYPE.clone()),
                    environment_record_type.clone(),
                )?;

                builder
                    .into_instructions()
                    .into_iter()
                    .chain(environment.iter().enumerate().map(|(index, (name, _))| {
                        DeconstructRecord::new(
                            environment_record_type.clone(),
                            environment_record.expression().clone(),
                            index,
                            name,
                        )
                        .into()
                    }))
                    .chain(block.instructions().iter().cloned())
                    .collect()
            },
            block.terminal_instruction().clone(),
        ),
        context.cps.result_type().clone(),
        CallingConvention::Tail,
        Linkage::Internal,
    ));

    Ok(Variable::new(name).into())
}

// The local variables should not include call results because they are
// passed as continuation arguments.
//
// TODO Sort fields to omit extra stack operations.
fn get_continuation_environment(
    instructions: &[Instruction],
    terminal_instruction: &TerminalInstruction,
    local_variables: &hamt::Map<String, Type>,
) -> Vec<(String, Type)> {
    [(
        CONTINUATION_ARGUMENT_NAME.into(),
        local_variables[CONTINUATION_ARGUMENT_NAME].clone(),
    )].into_iter()
    .chain(
        free_variable_collector::collect(instructions, terminal_instruction)
            .iter()
            .flat_map(|name| {
                local_variables
                    .get(name)
                    .map(|type_| (name.clone(), type_.clone()))
            }),
    )
    .collect()
}
