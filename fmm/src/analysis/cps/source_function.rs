use super::{context::CpsContext, error::CpsError, free_variable, stack};
use crate::{
    analysis::{cps::continuation_type, local_variable},
    build::{self, BuildError, InstructionBuilder},
    ir::*,
    types::{CallingConvention, Type},
};
use fnv::FnvHashMap;
use std::mem::{replace, take};

const STACK_ARGUMENT_NAME: &str = "_s";
const CONTINUATION_ARGUMENT_NAME: &str = "_k";

struct Context<'a> {
    cps: &'a CpsContext,
    function_definitions: Vec<FunctionDefinition>,
}

pub fn transform(context: &CpsContext, module: &mut Module) -> Result<(), CpsError> {
    let mut context = Context {
        cps: context,
        function_definitions: vec![],
    };

    for definition in module.function_definitions_mut() {
        transform_function_definition(&mut context, definition)?;
    }

    module
        .function_definitions_mut()
        .extend(context.function_definitions);

    Ok(())
}

fn transform_function_definition(
    context: &mut Context,
    definition: &mut FunctionDefinition,
) -> Result<(), CpsError> {
    if definition.type_().calling_convention() == CallingConvention::Source {
        let continuation_type =
            continuation_type::compile(definition.result_type(), context.cps.result_type());

        definition
            .arguments_mut()
            .insert(0, Argument::new(STACK_ARGUMENT_NAME, stack::type_()));
        definition.arguments_mut().insert(
            1,
            Argument::new(CONTINUATION_ARGUMENT_NAME, continuation_type.clone()),
        );

        *definition.result_type_mut() = context.cps.result_type().clone();
        *definition.options_mut() = definition
            .options()
            .clone()
            .set_calling_convention(CallingConvention::Tail);

        let mut local_variables = local_variable::collect(definition)
            .into_iter()
            .map(|(name, type_)| (name.to_owned(), type_))
            .collect::<FnvHashMap<_, _>>();

        local_variables.insert(CONTINUATION_ARGUMENT_NAME.into(), continuation_type.into());

        transform_block(context, definition.body_mut(), &local_variables)?;
    }

    Ok(())
}

fn transform_block(
    context: &mut Context,
    block: &mut Block,
    local_variables: &FnvHashMap<String, Type>,
) -> Result<(), BuildError> {
    let mut rest_instructions = Vec::with_capacity(block.instructions().len());
    let mut rest_terminal_instruction = replace(
        block.terminal_instruction_mut(),
        TerminalInstruction::Unreachable,
    );

    match block.instructions_mut().pop() {
        Some(Instruction::Call(mut call))
            if call.type_().calling_convention() == CallingConvention::Source
                && rest_terminal_instruction
                    .to_return()
                    .map(|return_| return_.expression() == &Variable::new(call.name()).into())
                    .unwrap_or_default() =>
        {
            let result_name = context.cps.name_generator().borrow_mut().generate();

            rest_terminal_instruction = Return::new(
                context.cps.result_type().clone(),
                Variable::new(&result_name),
            )
            .into();

            transform_call(
                &mut call,
                Variable::new(CONTINUATION_ARGUMENT_NAME),
                result_name,
            );

            rest_instructions.push(call.into());
        }
        instruction => {
            block.instructions_mut().extend(instruction);

            match rest_terminal_instruction {
                TerminalInstruction::Return(return_) => {
                    let result_name = context.cps.name_generator().borrow_mut().generate();

                    rest_instructions.push(
                        Call::new(
                            continuation_type::compile(return_.type_(), context.cps.result_type()),
                            Variable::new(CONTINUATION_ARGUMENT_NAME),
                            vec![
                                Variable::new(STACK_ARGUMENT_NAME).into(),
                                return_.expression().clone(),
                            ],
                            &result_name,
                        )
                        .into(),
                    );
                    rest_terminal_instruction = Return::new(
                        context.cps.result_type().clone(),
                        Variable::new(result_name),
                    )
                    .into();
                }
                TerminalInstruction::Branch(_) | TerminalInstruction::Unreachable => {}
            }
        }
    }

    for instruction in block.instructions_mut().drain(..).rev() {
        match instruction {
            Instruction::Call(mut call)
                if call.type_().calling_convention() == CallingConvention::Source =>
            {
                rest_instructions.reverse();

                let environment = get_continuation_environment(
                    &call,
                    &rest_instructions,
                    &rest_terminal_instruction,
                    local_variables,
                );
                let builder = InstructionBuilder::new(context.cps.name_generator());

                stack::push(
                    &builder,
                    build::variable(STACK_ARGUMENT_NAME, stack::type_()),
                    get_environment_record(&environment),
                )?;

                let result_name = context.cps.name_generator().borrow_mut().generate();

                let continuation = create_continuation(
                    context,
                    &call,
                    take(&mut rest_instructions),
                    replace(
                        &mut rest_terminal_instruction,
                        Return::new(
                            context.cps.result_type().clone(),
                            Variable::new(&result_name),
                        )
                        .into(),
                    ),
                    &environment,
                )?;

                transform_call(&mut call, continuation, result_name);

                rest_instructions.push(call.into());
                rest_instructions.extend(builder.into_instructions().into_iter().rev());

                continue;
            }
            Instruction::If(mut if_) => {
                transform_block(context, if_.then_mut(), local_variables)?;
                transform_block(context, if_.else_mut(), local_variables)?;

                rest_instructions.push(if_.into());
            }
            instruction => {
                rest_instructions.push(instruction);
            }
        }
    }

    rest_instructions.reverse();

    *block.instructions_mut() = rest_instructions;
    *block.terminal_instruction_mut() = rest_terminal_instruction;

    Ok(())
}

fn transform_call(call: &mut Call, continuation: impl Into<Expression>, result_name: String) {
    call.arguments_mut()
        .insert(0, Variable::new(STACK_ARGUMENT_NAME).into());
    call.arguments_mut().insert(1, continuation.into());
    *call.name_mut() = result_name;
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
    instructions: Vec<Instruction>,
    terminal_instruction: TerminalInstruction,
    environment: &[(String, Type)],
) -> Result<Expression, BuildError> {
    let name = context.cps.name_generator().borrow_mut().generate();

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
                            name,
                        )
                        .into()
                    }))
                    .chain(instructions)
                    .collect()
            },
            terminal_instruction,
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
fn get_continuation_environment(
    call: &Call,
    instructions: &[Instruction],
    terminal_instruction: &TerminalInstruction,
    local_variables: &FnvHashMap<String, Type>,
) -> Vec<(String, Type)> {
    free_variable::collect(instructions, terminal_instruction)
        .into_iter()
        .filter(|name| *name != call.name() && *name != STACK_ARGUMENT_NAME)
        .flat_map(|name| {
            local_variables
                .get(name)
                .map(|type_| (name.to_owned(), type_.clone()))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{analysis::format, types, types::void_type};
    use pretty_assertions::assert_eq;

    fn transform_module(mut module: Module) -> Result<Module, CpsError> {
        transform(&CpsContext::new(void_type()), &mut module)?;

        Ok(module)
    }

    #[test]
    fn transform_empty() {
        assert_eq!(
            transform_module(Module::new(vec![], vec![], vec![], vec![])),
            Ok(Module::new(vec![], vec![], vec![], vec![]))
        );
    }

    #[test]
    fn transform_no_instruction() {
        assert_eq!(
            transform_module(Module::new(
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
            transform_module(Module::new(
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
            transform_module(Module::new(
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
            &transform_module(Module::new(
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
            &transform_module(Module::new(
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
    fn transform_free_variable_between_two_calls() {
        insta::assert_snapshot!(format::format_module(
            &transform_module(Module::new(
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
