use crate::{
    analysis::local_variable,
    build::{self, InstructionBuilder},
    ir::*,
    types::{CallingConvention, Type},
};
use fnv::FnvHashMap;
use std::mem::{replace, take};

pub fn transform(module: &mut Module) {
    for definition in module.function_definitions_mut() {
        transform_function_definition(definition);
    }
}

fn transform_function_definition(definition: &mut FunctionDefinition) {
    if definition.type_().calling_convention() != CallingConvention::Source {
        return;
    }

    let local_variables = local_variable::collect(definition);

    transform_block(definition.body_mut(), &local_variables);
}

fn transform_block(block: &mut Block, local_variables: &FnvHashMap<String, Type>) {
    let mut rest_instructions = take(block.instructions_mut());
    rest_instructions.reverse();

    while let Some(instruction) = rest_instructions.pop() {
        match instruction {
            Instruction::Call(mut call)
                if call.type_().calling_convention() == CallingConvention::Source =>
            {
                rest_instructions.reverse();

                let result_name = context.cps.name_generator().borrow_mut().generate();
                let terminal_instruction = replace(
                    block.terminal_instruction_mut(),
                    Return::new(
                        context.cps.result_type().clone(),
                        Variable::new(&result_name),
                    )
                    .into(),
                );

                let continuation = if rest_instructions.is_empty()
                    && terminal_instruction
                        .to_return()
                        .map(|return_| return_.expression() == &Variable::new(call.name()).into())
                        .unwrap_or_default()
                {
                    Variable::new(CONTINUATION_ARGUMENT_NAME).into()
                } else {
                    let mut continuation_block =
                        Block::new(rest_instructions, terminal_instruction);
                    transform_block(context, &mut continuation_block, local_variables)?;

                    let environment = create_continuation_environment(
                        &call,
                        &continuation_block,
                        local_variables,
                    );
                    let continuation =
                        create_continuation(context, &call, continuation_block, &environment)?;

                    let builder = InstructionBuilder::new(context.cps.name_generator());
                    stack::push(
                        &builder,
                        build::variable(STACK_ARGUMENT_NAME, stack::type_()),
                        create_environment_record(&environment),
                    )?;
                    block.instructions_mut().extend(builder.into_instructions());

                    continuation
                };

                transform_call(&mut call, continuation, result_name);
                block.instructions_mut().push(call.into());

                return Ok(());
            }
            Instruction::If(mut if_) => {
                transform_block(context, if_.then_mut(), local_variables)?;
                transform_block(context, if_.else_mut(), local_variables)?;

                block.instructions_mut().push(if_.into());
            }
            instruction => block.instructions_mut().push(instruction),
        }
    }

    if let TerminalInstruction::Return(return_) = block.terminal_instruction_mut() {
        let name = context.cps.name_generator().borrow_mut().generate();
        let type_ = replace(return_.type_mut(), context.cps.result_type().clone());
        let expression = replace(return_.expression_mut(), Variable::new(&name).into());

        block.instructions_mut().push(
            Call::new(
                continuation_type::compile(&type_, context.cps.result_type()),
                Variable::new(CONTINUATION_ARGUMENT_NAME),
                vec![Variable::new(STACK_ARGUMENT_NAME).into(), expression],
                name,
            )
            .into(),
        );
    }
}

fn create_continuation_environment<'a>(
    call: &Call,
    block: &Block,
    local_variables: &'a FnvHashMap<String, Type>,
) -> Vec<(&'a str, &'a Type)> {
    free_variable::collect(block.instructions(), block.terminal_instruction())
        .into_iter()
        .filter(|name| *name != call.name())
        .flat_map(|name| {
            local_variables
                .get_key_value(name)
                .map(|(name, type_)| (name.as_str(), type_))
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

    #[test]
    fn transform_two_calls_with_shared_free_variable() {
        insta::assert_snapshot!(format::format_module(
            &transform_module(Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![Argument::new("x", types::Primitive::Float64)],
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
                        Return::new(types::Primitive::Float64, Variable::new("x")),
                    ),
                    Default::default()
                )],
            ))
            .unwrap()
        ));
    }

    #[test]
    fn transform_two_calls_with_two_shared_free_variables() {
        insta::assert_snapshot!(format::format_module(
            &transform_module(Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![
                        Argument::new("x", types::Primitive::Float64),
                        Argument::new("y", types::Primitive::Float64)
                    ],
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
                                "p",
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
                                "q",
                            )
                            .into()
                        ],
                        Return::new(
                            types::Primitive::Float64,
                            ArithmeticOperation::new(
                                types::Primitive::Float64,
                                ArithmeticOperator::Add,
                                Variable::new("x"),
                                Variable::new("y")
                            )
                        ),
                    ),
                    Default::default()
                )],
            ))
            .unwrap()
        ));
    }

    // TODO Fix a stack element order.
    #[test]
    fn transform_two_calls_with_free_variables_shared_and_dropped() {
        insta::assert_snapshot!(format::format_module(
            &transform_module(Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![
                        Argument::new("x", types::Primitive::Float64),
                        Argument::new("y", types::Primitive::Float64)
                    ],
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
                                "p",
                            )
                            .into(),
                            Call::new(
                                types::Function::new(
                                    vec![types::Primitive::Float64.into()],
                                    types::Primitive::Float64,
                                    types::CallingConvention::Source
                                ),
                                Variable::new("g"),
                                vec![Variable::new("y").into()],
                                "q",
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
}
