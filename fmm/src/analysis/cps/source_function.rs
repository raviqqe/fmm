use super::{context::CpsContext, error::CpsError, free_variable, stack};
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
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .chain(context.function_definitions)
            .collect(),
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
                    Argument::new(STACK_ARGUMENT_NAME, stack::type_()),
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
                (vec![], terminal_instruction.clone())
            }
        },
        [instruction, ..] => {
            let instructions = &instructions[1..];

            if let Instruction::Call(call) = instruction {
                if call.type_().calling_convention() == CallingConvention::Source {
                    let result_name = context.cps.name_generator().borrow_mut().generate();
                    let return_ = Return::new(
                        context.cps.result_type().clone(),
                        Variable::new(&result_name),
                    )
                    .into();

                    if instructions.is_empty()
                        && terminal_instruction
                            .to_return()
                            .map(|return_| {
                                return_.expression() == &Variable::new(call.name()).into()
                            })
                            .unwrap_or_default()
                    {
                        return Ok((
                            vec![transform_call(
                                call,
                                Variable::new(CONTINUATION_ARGUMENT_NAME),
                                &result_name,
                            )
                            .into()],
                            return_,
                        ));
                    }

                    let environment = get_continuation_environment(
                        instructions,
                        terminal_instruction,
                        local_variables,
                    );
                    let builder = InstructionBuilder::new(context.cps.name_generator());

                    stack::push(
                        &builder,
                        build::variable(STACK_ARGUMENT_NAME, stack::type_()),
                        get_environment_record(&environment),
                    )?;

                    return Ok((
                        builder
                            .into_instructions()
                            .into_iter()
                            .chain([transform_call(
                                call,
                                create_continuation(
                                    context,
                                    call,
                                    instructions,
                                    terminal_instruction,
                                    &environment,
                                )?,
                                &result_name,
                            )
                            .into()])
                            .collect(),
                        return_,
                    ));
                }
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
                [if let Instruction::If(if_) = instruction {
                    If::new(
                        if_.type_().clone(),
                        if_.condition().clone(),
                        transform_block(context, if_.then(), local_variables)?,
                        transform_block(context, if_.else_(), local_variables)?,
                        if_.name(),
                    )
                    .into()
                } else {
                    instruction.clone()
                }]
                .into_iter()
                .chain(instructions)
                .collect(),
                terminal_instruction,
            )
        }
    })
}

fn transform_call(call: &Call, continuation: impl Into<Expression>, result_name: &str) -> Call {
    Call::new(
        call.type_().clone(),
        call.function().clone(),
        [
            Variable::new(STACK_ARGUMENT_NAME).into(),
            continuation.into(),
        ]
        .into_iter()
        .chain(call.arguments().iter().cloned())
        .collect(),
        result_name,
    )
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
            Argument::new(STACK_ARGUMENT_NAME, stack::type_()),
            Argument::new(call.name(), call.type_().result().clone()),
        ],
        context.cps.result_type().clone(),
        Block::new(
            {
                let builder = InstructionBuilder::new(context.cps.name_generator());

                let environment_record_type = get_environment_record(environment).type_().clone();
                let environment_record = stack::pop(
                    &builder,
                    build::variable(STACK_ARGUMENT_NAME, stack::type_()),
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
    [CONTINUATION_ARGUMENT_NAME]
        .into_iter()
        .chain(free_variable::collect(instructions, terminal_instruction))
        .flat_map(|name| local_variables.get(name).map(|type_| (name, type_.clone())))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{analysis::format, types, types::void_type};
    use pretty_assertions::assert_eq;

    fn transform_module(module: &Module) -> Result<Module, CpsError> {
        transform(&CpsContext::new(void_type()), module)
    }

    #[test]
    fn transform_empty() {
        assert_eq!(
            transform_module(&Module::new(vec![], vec![], vec![], vec![])),
            Ok(Module::new(vec![], vec![], vec![], vec![]))
        );
    }

    #[test]
    fn transform_no_instruction() {
        assert_eq!(
            transform_module(&Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(vec![], TerminalInstruction::Unreachable,),
                    Default::default()
                )],
            )),
            Ok(Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![
                        Argument::new(STACK_ARGUMENT_NAME, stack::type_()),
                        Argument::new(
                            CONTINUATION_ARGUMENT_NAME,
                            continuation_type::compile(
                                &types::Primitive::Float64.into(),
                                &void_type().into()
                            )
                        )
                    ],
                    void_type(),
                    Block::new(vec![], TerminalInstruction::Unreachable,),
                    FunctionDefinitionOptions::new()
                        .set_calling_convention(types::CallingConvention::Tail)
                )],
            ))
        );
    }

    #[test]
    fn transform_if() {
        assert_eq!(
            transform_module(&Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![If::new(
                            void_type(),
                            Primitive::Boolean(true),
                            Block::new(vec![], TerminalInstruction::Unreachable),
                            Block::new(vec![], TerminalInstruction::Unreachable),
                            "_",
                        )
                        .into()],
                        TerminalInstruction::Unreachable,
                    ),
                    Default::default()
                )],
            )),
            Ok(Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![
                        Argument::new(STACK_ARGUMENT_NAME, stack::type_()),
                        Argument::new(
                            CONTINUATION_ARGUMENT_NAME,
                            continuation_type::compile(
                                &types::Primitive::Float64.into(),
                                &void_type().into()
                            )
                        )
                    ],
                    void_type(),
                    Block::new(
                        vec![If::new(
                            void_type(),
                            Primitive::Boolean(true),
                            Block::new(vec![], TerminalInstruction::Unreachable),
                            Block::new(vec![], TerminalInstruction::Unreachable),
                            "_",
                        )
                        .into()],
                        TerminalInstruction::Unreachable,
                    ),
                    FunctionDefinitionOptions::new()
                        .set_calling_convention(types::CallingConvention::Tail)
                )],
            ))
        );
    }

    #[test]
    fn transform_tail_call() {
        assert_eq!(
            transform_module(&Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![Call::new(
                            types::Function::new(
                                vec![],
                                types::Primitive::Float64,
                                types::CallingConvention::Source
                            ),
                            Variable::new("f"),
                            vec![],
                            "x",
                        )
                        .into()],
                        Return::new(types::Primitive::Float64, Variable::new("x")),
                    ),
                    Default::default()
                )],
            )),
            Ok(Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![
                        Argument::new(STACK_ARGUMENT_NAME, stack::type_()),
                        Argument::new(
                            CONTINUATION_ARGUMENT_NAME,
                            continuation_type::compile(
                                &types::Primitive::Float64.into(),
                                &void_type().into()
                            )
                        )
                    ],
                    void_type(),
                    Block::new(
                        vec![Call::new(
                            types::Function::new(
                                vec![],
                                types::Primitive::Float64,
                                types::CallingConvention::Source
                            ),
                            Variable::new("f"),
                            vec![Variable::new("_s").into(), Variable::new("_k").into()],
                            "_k_0",
                        )
                        .into()],
                        Return::new(void_type(), Variable::new("_k_0")),
                    ),
                    FunctionDefinitionOptions::new()
                        .set_calling_convention(types::CallingConvention::Tail)
                )],
            ))
        );
    }

    #[test]
    fn transform_non_tail_call() {
        insta::assert_snapshot!(format::format_module(
            &transform_module(&Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![
                            Call::new(
                                types::Function::new(
                                    vec![],
                                    types::Primitive::Float64,
                                    types::CallingConvention::Source
                                ),
                                Variable::new("f"),
                                vec![],
                                "x",
                            )
                            .into(),
                            Store::new(
                                types::Primitive::Float64,
                                Undefined::new(types::Primitive::Float64),
                                Variable::new("x")
                            )
                            .into()
                        ],
                        Return::new(types::Primitive::Float64, Variable::new("x")),
                    ),
                    Default::default()
                )],
            ))
            .unwrap()
        ));
    }

    #[test]
    fn transform_non_tail_call_in_if() {
        insta::assert_snapshot!(format::format_module(
            &transform_module(&Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![If::new(
                            void_type(),
                            Primitive::Boolean(true),
                            Block::new(
                                vec![
                                    Call::new(
                                        types::Function::new(
                                            vec![],
                                            types::Primitive::Float64,
                                            types::CallingConvention::Source
                                        ),
                                        Variable::new("f"),
                                        vec![],
                                        "x",
                                    )
                                    .into(),
                                    Store::new(
                                        types::Primitive::Float64,
                                        Undefined::new(types::Primitive::Float64),
                                        Variable::new("x")
                                    )
                                    .into()
                                ],
                                Return::new(types::Primitive::Float64, Variable::new("x"))
                            ),
                            Block::new(vec![], TerminalInstruction::Unreachable),
                            "_",
                        )
                        .into()],
                        TerminalInstruction::Unreachable,
                    ),
                    Default::default()
                )],
            ))
            .unwrap()
        ));
    }

    #[test]
    fn transform_two_calls_with_free_variable_inbetween() {
        insta::assert_snapshot!(format::format_module(
            &transform_module(&Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![
                            Call::new(
                                types::Function::new(
                                    vec![],
                                    types::Primitive::Float64,
                                    types::CallingConvention::Source
                                ),
                                Variable::new("f"),
                                vec![],
                                "x",
                            )
                            .into(),
                            Load::new(
                                types::Primitive::Float64,
                                Undefined::new(types::Pointer::new(types::Primitive::Float64)),
                                "y",
                            )
                            .into(),
                            Call::new(
                                types::Function::new(
                                    vec![],
                                    types::Primitive::Float64,
                                    types::CallingConvention::Source
                                ),
                                Variable::new("f"),
                                vec![],
                                "z",
                            )
                            .into()
                        ],
                        Return::new(
                            types::Primitive::Float64,
                            ArithmeticOperation::new(
                                types::Primitive::Float64,
                                ArithmeticOperator::Add,
                                Variable::new("y"),
                                Variable::new("z")
                            )
                        ),
                    ),
                    Default::default()
                )],
            ))
            .unwrap()
        ));
    }
}
