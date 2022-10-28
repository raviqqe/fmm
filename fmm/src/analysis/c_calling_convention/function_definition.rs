use super::{context::Context, type_};
use crate::{
    ir::*,
    types::{self, void_type, Type},
};

pub fn transform(context: &Context, definition: &mut FunctionDefinition) {
    if definition.type_().calling_convention() != types::CallingConvention::Target {
        return;
    }

    let result_pointer = if type_::is_memory_class(context, definition.result_type()) {
        Some((
            pointer_name(definition.name()),
            definition.result_type().clone(),
        ))
    } else {
        None
    };

    let mut arguments = Vec::with_capacity(definition.arguments().len());
    let mut instructions = Vec::with_capacity(definition.body().instructions().len());

    if let Some((name, type_)) = &result_pointer {
        arguments.push(Argument::new(name, type_::transform_memory_class(type_)));
    }

    for argument in definition.arguments_mut().drain(..) {
        if type_::is_memory_class(context, argument.type_()) {
            arguments.push(Argument::new(
                pointer_name(argument.name()),
                type_::transform_memory_class(argument.type_()),
            ));
            instructions.push(
                Load::new(
                    argument.type_().clone(),
                    Variable::new(pointer_name(argument.name())),
                    argument.name(),
                )
                .into(),
            );
        } else {
            arguments.push(argument);
        }
    }

    *definition.arguments_mut() = arguments;

    if result_pointer.is_some() {
        *definition.result_type_mut() = void_type().into()
    }

    transform_block(definition.body_mut(), instructions, result_pointer.as_ref());
}

fn transform_block(
    block: &mut Block,
    mut instructions: Vec<Instruction>,
    result_pointer: Option<&(String, Type)>,
) {
    for mut instruction in block.instructions_mut().drain(..) {
        transform_instruction(&mut instruction, result_pointer);

        instructions.push(instruction);
    }

    if let (TerminalInstruction::Return(return_), Some((pointer_name, type_))) =
        (block.terminal_instruction(), result_pointer)
    {
        instructions.push(
            Store::new(
                type_.clone(),
                return_.expression().clone(),
                Variable::new(&**pointer_name),
            )
            .into(),
        );
        *block.terminal_instruction_mut() = Return::new(void_type(), void_value()).into();
    }

    *block.instructions_mut() = instructions;
}

fn transform_instruction(instruction: &mut Instruction, result_pointer: Option<&(String, Type)>) {
    if let Instruction::If(if_) = instruction {
        transform_block(if_.then_mut(), vec![], result_pointer);
        transform_block(if_.else_mut(), vec![], result_pointer);
    }
}

fn pointer_name(name: &str) -> String {
    format!("{}_p", name)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    const WORD_BYTES: usize = 8;

    fn transform_definition(mut definition: FunctionDefinition) -> FunctionDefinition {
        transform(&Context::new(WORD_BYTES), &mut definition);

        definition
    }

    #[test]
    fn transform_compatible() {
        let definition = FunctionDefinition::new(
            "f",
            vec![Argument::new("x", types::Primitive::Integer64)],
            void_type(),
            Block::new(vec![], Return::new(void_type(), void_value())),
            FunctionDefinitionOptions::new()
                .set_calling_convention(types::CallingConvention::Target),
        );

        assert_eq!(transform_definition(definition.clone()), definition);
    }

    #[test]
    fn transform_arguments() {
        let record_type = types::Record::new(vec![
            types::Primitive::Integer64.into(),
            types::Primitive::Integer64.into(),
            types::Primitive::Integer64.into(),
        ]);

        assert_eq!(
            transform_definition(FunctionDefinition::new(
                "f",
                vec![Argument::new("x", record_type.clone())],
                void_type(),
                Block::new(vec![], Return::new(void_type(), void_value()),),
                FunctionDefinitionOptions::new()
                    .set_calling_convention(types::CallingConvention::Target),
            )),
            FunctionDefinition::new(
                "f",
                vec![Argument::new(
                    "x_p",
                    types::Pointer::new(record_type.clone())
                )],
                void_type(),
                Block::new(
                    vec![Load::new(record_type, Variable::new("x_p"), "x").into()],
                    Return::new(void_type(), void_value()),
                ),
                FunctionDefinitionOptions::new()
                    .set_calling_convention(types::CallingConvention::Target),
            )
        );
    }

    #[test]
    fn transform_result() {
        let record_type = types::Record::new(vec![
            types::Primitive::Integer64.into(),
            types::Primitive::Integer64.into(),
            types::Primitive::Integer64.into(),
        ]);

        assert_eq!(
            transform_definition(FunctionDefinition::new(
                "f",
                vec![],
                record_type.clone(),
                Block::new(
                    vec![],
                    Return::new(record_type.clone(), Undefined::new(record_type.clone())),
                ),
                FunctionDefinitionOptions::new()
                    .set_calling_convention(types::CallingConvention::Target),
            )),
            FunctionDefinition::new(
                "f",
                vec![Argument::new(
                    "f_p",
                    types::Pointer::new(record_type.clone())
                )],
                void_type(),
                Block::new(
                    vec![Store::new(
                        record_type.clone(),
                        Undefined::new(record_type),
                        Variable::new("f_p")
                    )
                    .into()],
                    Return::new(void_type(), void_value()),
                ),
                FunctionDefinitionOptions::new()
                    .set_calling_convention(types::CallingConvention::Target),
            )
        );
    }

    #[test]
    fn transform_result_with_argument() {
        let record_type = types::Record::new(vec![
            types::Primitive::Integer64.into(),
            types::Primitive::Integer64.into(),
            types::Primitive::Integer64.into(),
        ]);

        assert_eq!(
            transform_definition(FunctionDefinition::new(
                "f",
                vec![Argument::new("x", types::Primitive::PointerInteger)],
                record_type.clone(),
                Block::new(
                    vec![],
                    Return::new(record_type.clone(), Undefined::new(record_type.clone())),
                ),
                FunctionDefinitionOptions::new()
                    .set_calling_convention(types::CallingConvention::Target),
            )),
            FunctionDefinition::new(
                "f",
                vec![
                    Argument::new("f_p", types::Pointer::new(record_type.clone())),
                    Argument::new("x", types::Primitive::PointerInteger)
                ],
                void_type(),
                Block::new(
                    vec![Store::new(
                        record_type.clone(),
                        Undefined::new(record_type),
                        Variable::new("f_p")
                    )
                    .into()],
                    Return::new(void_type(), void_value()),
                ),
                FunctionDefinitionOptions::new()
                    .set_calling_convention(types::CallingConvention::Target),
            )
        );
    }

    #[test]
    fn transform_if() {
        let record_type = types::Record::new(vec![
            types::Primitive::Integer64.into(),
            types::Primitive::Integer64.into(),
            types::Primitive::Integer64.into(),
        ]);

        assert_eq!(
            transform_definition(FunctionDefinition::new(
                "f",
                vec![],
                record_type.clone(),
                Block::new(
                    vec![If::new(
                        void_type(),
                        Primitive::Boolean(true),
                        Block::new(
                            vec![],
                            Return::new(record_type.clone(), Undefined::new(record_type.clone())),
                        ),
                        Block::new(
                            vec![],
                            Return::new(record_type.clone(), Undefined::new(record_type.clone())),
                        ),
                        "x"
                    )
                    .into()],
                    TerminalInstruction::Unreachable
                ),
                FunctionDefinitionOptions::new()
                    .set_calling_convention(types::CallingConvention::Target),
            )),
            FunctionDefinition::new(
                "f",
                vec![Argument::new(
                    "f_p",
                    types::Pointer::new(record_type.clone())
                )],
                void_type(),
                Block::new(
                    vec![If::new(
                        void_type(),
                        Primitive::Boolean(true),
                        Block::new(
                            vec![Store::new(
                                record_type.clone(),
                                Undefined::new(record_type.clone()),
                                Variable::new("f_p")
                            )
                            .into()],
                            Return::new(void_type(), void_value()),
                        ),
                        Block::new(
                            vec![Store::new(
                                record_type.clone(),
                                Undefined::new(record_type),
                                Variable::new("f_p")
                            )
                            .into()],
                            Return::new(void_type(), void_value()),
                        ),
                        "x"
                    )
                    .into(),],
                    TerminalInstruction::Unreachable
                ),
                FunctionDefinitionOptions::new()
                    .set_calling_convention(types::CallingConvention::Target),
            )
        );
    }
}
