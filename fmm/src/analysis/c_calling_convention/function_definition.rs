use super::{context::Context, type_};
use crate::{
    ir::*,
    types::{self, void_type, Type},
};

pub fn transform(context: &Context, definition: &FunctionDefinition) -> FunctionDefinition {
    if definition.type_().calling_convention() == types::CallingConvention::Target {
        let result_pointer_name = pointer_name(definition.name());
        let result_pointer = if type_::is_memory_class(context, definition.result_type()) {
            Some((&*result_pointer_name, definition.result_type()))
        } else {
            None
        };
        let block = transform_block(definition.body(), result_pointer);

        FunctionDefinition::new(
            definition.name(),
            result_pointer
                .map(|(name, _)| {
                    Argument::new(name, types::Pointer::new(definition.result_type().clone()))
                })
                .into_iter()
                .chain(definition.arguments().iter().map(|argument| {
                    Argument::new(
                        if type_::is_memory_class(context, argument.type_()) {
                            pointer_name(argument.name())
                        } else {
                            argument.name().into()
                        },
                        type_::transform(context, argument.type_()),
                    )
                }))
                .collect(),
            if result_pointer.is_some() {
                void_type().into()
            } else {
                definition.result_type().clone()
            },
            Block::new(
                definition
                    .arguments()
                    .iter()
                    .flat_map(|argument| {
                        if type_::is_memory_class(context, argument.type_()) {
                            Some(
                                Load::new(
                                    argument.type_().clone(),
                                    Variable::new(pointer_name(argument.name())),
                                    argument.name(),
                                )
                                .into(),
                            )
                        } else {
                            None
                        }
                    })
                    .chain(block.instructions().iter().cloned())
                    .collect(),
                block.terminal_instruction().clone(),
            ),
            definition.options().clone(),
        )
    } else {
        definition.clone()
    }
}

fn transform_block(block: &Block, result_pointer: Option<(&str, &Type)>) -> Block {
    let mut instructions = vec![];

    for instruction in block.instructions() {
        instructions.push(transform_instruction(instruction, result_pointer));
    }

    if let (TerminalInstruction::Return(return_), Some((pointer_name, type_))) =
        (block.terminal_instruction(), result_pointer)
    {
        instructions.push(
            Store::new(
                type_.clone(),
                return_.expression().clone(),
                Variable::new(pointer_name),
            )
            .into(),
        );

        Block::new(instructions, Return::new(void_type(), void_value()))
    } else {
        Block::new(instructions, block.terminal_instruction().clone())
    }
}

fn transform_instruction(
    instruction: &Instruction,
    result_pointer: Option<(&str, &Type)>,
) -> Instruction {
    match instruction {
        Instruction::If(if_) => If::new(
            if_.type_().clone(),
            if_.condition().clone(),
            transform_block(if_.then(), result_pointer),
            transform_block(if_.else_(), result_pointer),
            if_.name(),
        )
        .into(),
        _ => instruction.clone(),
    }
}

fn pointer_name(name: &str) -> String {
    format!("{}_c_pointer", name)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    const WORD_BYTES: usize = 8;

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

        assert_eq!(
            transform(&Context::new(WORD_BYTES), &definition,),
            definition
        );
    }

    #[test]
    fn transform_arguments() {
        let record_type = types::Record::new(vec![
            types::Primitive::Integer64.into(),
            types::Primitive::Integer64.into(),
            types::Primitive::Integer64.into(),
        ]);

        assert_eq!(
            transform(
                &Context::new(WORD_BYTES),
                &FunctionDefinition::new(
                    "f",
                    vec![Argument::new("x", record_type.clone())],
                    void_type(),
                    Block::new(vec![], Return::new(void_type(), void_value()),),
                    FunctionDefinitionOptions::new()
                        .set_calling_convention(types::CallingConvention::Target),
                )
            ),
            FunctionDefinition::new(
                "f",
                vec![Argument::new(
                    "x_c_pointer",
                    types::Pointer::new(record_type.clone())
                )],
                void_type(),
                Block::new(
                    vec![Load::new(record_type, Variable::new("x_c_pointer"), "x").into()],
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
            transform(
                &Context::new(WORD_BYTES),
                &FunctionDefinition::new(
                    "f",
                    vec![],
                    record_type.clone(),
                    Block::new(
                        vec![],
                        Return::new(record_type.clone(), Undefined::new(record_type.clone())),
                    ),
                    FunctionDefinitionOptions::new()
                        .set_calling_convention(types::CallingConvention::Target),
                )
            ),
            FunctionDefinition::new(
                "f",
                vec![Argument::new(
                    "f_c_pointer",
                    types::Pointer::new(record_type.clone())
                )],
                void_type(),
                Block::new(
                    vec![Store::new(
                        record_type.clone(),
                        Undefined::new(record_type),
                        Variable::new("f_c_pointer")
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
            transform(
                &Context::new(WORD_BYTES),
                &FunctionDefinition::new(
                    "f",
                    vec![Argument::new("x", types::Primitive::PointerInteger)],
                    record_type.clone(),
                    Block::new(
                        vec![],
                        Return::new(record_type.clone(), Undefined::new(record_type.clone())),
                    ),
                    FunctionDefinitionOptions::new()
                        .set_calling_convention(types::CallingConvention::Target),
                )
            ),
            FunctionDefinition::new(
                "f",
                vec![
                    Argument::new("f_c_pointer", types::Pointer::new(record_type.clone())),
                    Argument::new("x", types::Primitive::PointerInteger)
                ],
                void_type(),
                Block::new(
                    vec![Store::new(
                        record_type.clone(),
                        Undefined::new(record_type),
                        Variable::new("f_c_pointer")
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
            transform(
                &Context::new(WORD_BYTES),
                &FunctionDefinition::new(
                    "f",
                    vec![],
                    record_type.clone(),
                    Block::new(
                        vec![If::new(
                            void_type(),
                            Primitive::Boolean(true),
                            Block::new(
                                vec![],
                                Return::new(
                                    record_type.clone(),
                                    Undefined::new(record_type.clone())
                                ),
                            ),
                            Block::new(
                                vec![],
                                Return::new(
                                    record_type.clone(),
                                    Undefined::new(record_type.clone())
                                ),
                            ),
                            "x"
                        )
                        .into()],
                        TerminalInstruction::Unreachable
                    ),
                    FunctionDefinitionOptions::new()
                        .set_calling_convention(types::CallingConvention::Target),
                )
            ),
            FunctionDefinition::new(
                "f",
                vec![Argument::new(
                    "f_c_pointer",
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
                                Variable::new("f_c_pointer")
                            )
                            .into()],
                            Return::new(void_type(), void_value()),
                        ),
                        Block::new(
                            vec![Store::new(
                                record_type.clone(),
                                Undefined::new(record_type),
                                Variable::new("f_c_pointer")
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
