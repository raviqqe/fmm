use super::{
    context::CpsContext,
    error::CpsError,
    free_variable,
    stack::{pop_from_stack, push_to_stack, stack_type},
};
use crate::{
    analysis::cps::continuation_type,
    build::{self, BuildError, InstructionBuilder},
    ir::*,
    types::{CallingConvention, Type},
};

const STACK_ARGUMENT_NAME: &str = "_s";
const CONTINUATION_ARGUMENT_NAME: &str = "_k";

struct Context<'a> {
    cps: &'a CpsContext,
    function_definitions: Vec<FunctionDefinition>,
}

pub fn transform(context: &CpsContext, module: &Module) -> Result<Module, CpsError> {
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
) -> Result<FunctionDefinition, CpsError> {
    Ok(
        if definition.type_().calling_convention() == CallingConvention::Source {
            let continuation_type =
                continuation_type::compile(definition.result_type(), context.cps.result_type());

            FunctionDefinition::new(
                definition.name(),
                [
                    Argument::new(STACK_ARGUMENT_NAME, stack_type()),
                    Argument::new(CONTINUATION_ARGUMENT_NAME, continuation_type.clone()),
                ]
                .into_iter()
                .chain(definition.arguments().iter().cloned())
                .collect(),
                context.cps.result_type().clone(),
                transform_block(
                    context,
                    definition.body(),
                    &definition
                        .arguments()
                        .iter()
                        .map(|argument| (argument.name(), argument.type_().clone()))
                        .chain([(CONTINUATION_ARGUMENT_NAME, continuation_type.into())])
                        .collect(),
                )?,
                definition
                    .options()
                    .clone()
                    .set_calling_convention(CallingConvention::Tail),
            )
        } else {
            definition.clone()
        },
    )
}

fn transform_block(
    context: &mut Context,
    block: &Block,
    local_variables: &hamt::Map<&str, Type>,
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
    local_variables: &hamt::Map<&str, Type>,
) -> Result<(Vec<Instruction>, TerminalInstruction), BuildError> {
    Ok(match instructions {
        [] => match terminal_instruction {
            TerminalInstruction::Return(return_) => {
                let result_name = context.cps.name_generator().borrow_mut().generate();

                (
                    vec![Call::new(
                        continuation_type::compile(return_.type_(), context.cps.result_type()),
                        Variable::new(CONTINUATION_ARGUMENT_NAME),
                        vec![
                            Variable::new(STACK_ARGUMENT_NAME).into(),
                            return_.expression().clone(),
                        ],
                        &result_name,
                    )
                    .into()],
                    Return::new(
                        context.cps.result_type().clone(),
                        Variable::new(result_name),
                    )
                    .into(),
                )
            }
            TerminalInstruction::Branch(_) | TerminalInstruction::Unreachable => {
                (vec![], TerminalInstruction::Unreachable)
            }
        },
        [instruction, ..] => {
            let instructions = &instructions[1..];

            if let Instruction::Call(call) = instruction {
                if call.type_().calling_convention() == CallingConvention::Source {
                    let is_tail_call = instructions.is_empty()
                        && terminal_instruction
                            .to_return()
                            .map(|return_| {
                                return_.expression() == &Variable::new(call.name()).into()
                            })
                            .unwrap_or_default();
                    let environment = get_continuation_environment(
                        instructions,
                        terminal_instruction,
                        local_variables,
                    );
                    let builder = InstructionBuilder::new(context.cps.name_generator());

                    if !is_tail_call {
                        push_to_stack(
                            &builder,
                            build::variable(STACK_ARGUMENT_NAME, stack_type()),
                            get_environment_record(&environment),
                        )?;
                    }

                    let result_name = context.cps.name_generator().borrow_mut().generate();

                    return Ok((
                        builder
                            .into_instructions()
                            .into_iter()
                            .chain([Call::new(
                                call.type_().clone(),
                                call.function().clone(),
                                [
                                    Variable::new(STACK_ARGUMENT_NAME).into(),
                                    if is_tail_call {
                                        Variable::new(CONTINUATION_ARGUMENT_NAME).into()
                                    } else {
                                        create_continuation(
                                            context,
                                            call,
                                            instructions,
                                            terminal_instruction,
                                            &environment,
                                        )?
                                    },
                                ]
                                .into_iter()
                                .chain(call.arguments().iter().cloned())
                                .collect(),
                                &result_name,
                            )
                            .into()])
                            .collect(),
                        Return::new(
                            context.cps.result_type().clone(),
                            Variable::new(result_name),
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
                &if let Some((name, type_)) = instruction.value() {
                    local_variables.insert(name, type_)
                } else {
                    local_variables.clone()
                },
            )?;

            (
                [instruction.clone()]
                    .into_iter()
                    .chain(instructions)
                    .collect(),
                terminal_instruction,
            )
        }
    })
}

fn get_environment_record(environment: &[(&str, Type)]) -> Record {
    build::record(
        environment
            .iter()
            .map(|(name, type_)| build::variable(*name, type_.clone()))
            .collect(),
    )
}

fn create_continuation(
    context: &mut Context,
    call: &Call,
    instructions: &[Instruction],
    terminal_instruction: &TerminalInstruction,
    environment: &[(&str, Type)],
) -> Result<Expression, BuildError> {
    let name = context.cps.name_generator().borrow_mut().generate();
    let block = transform_block(
        context,
        &Block::new(instructions.to_vec(), terminal_instruction.clone()),
        &environment
            .iter()
            .cloned()
            .chain([(call.name(), call.type_().result().clone())])
            .collect(),
    )?;

    context.function_definitions.push(FunctionDefinition::new(
        &name,
        vec![
            Argument::new(STACK_ARGUMENT_NAME, stack_type()),
            Argument::new(call.name(), call.type_().result().clone()),
        ],
        context.cps.result_type().clone(),
        Block::new(
            {
                let builder = InstructionBuilder::new(context.cps.name_generator());

                let environment_record_type = get_environment_record(environment).type_().clone();
                let environment_record = pop_from_stack(
                    &builder,
                    build::variable(STACK_ARGUMENT_NAME, stack_type()),
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
                            *name,
                        )
                        .into()
                    }))
                    .chain(block.instructions().iter().cloned())
                    .collect()
            },
            block.terminal_instruction().clone(),
        ),
        FunctionDefinitionOptions::new()
            .set_address_named(false)
            .set_calling_convention(CallingConvention::Tail)
            .set_linkage(Linkage::Internal),
    ));

    Ok(Variable::new(name).into())
}

// Local variables should not include call results because they are
// passed as continuation arguments.
//
// TODO Sort fields to omit extra stack operations.
fn get_continuation_environment<'a>(
    instructions: &'a [Instruction],
    terminal_instruction: &'a TerminalInstruction,
    local_variables: &hamt::Map<&str, Type>,
) -> Vec<(&'a str, Type)> {
    [(
        CONTINUATION_ARGUMENT_NAME,
        local_variables[CONTINUATION_ARGUMENT_NAME].clone(),
    )]
    .into_iter()
    .chain(
        free_variable::collect(instructions, terminal_instruction)
            .iter()
            .flat_map(|&name| local_variables.get(name).map(|type_| (name, type_.clone()))),
    )
    .collect()
}
