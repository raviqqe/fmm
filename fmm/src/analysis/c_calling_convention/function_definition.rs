use super::{context::Context, type_};
use crate::{
    ir::*,
    types::{self, void_type, Type},
};

pub fn transform(context: &Context, definition: &FunctionDefinition) -> FunctionDefinition {
    if definition.type_().calling_convention() == types::CallingConvention::Target {
        let is_result_memory = type_::is_memory_class(context, definition.result_type());
        let pointer_name = if is_result_memory {
            Some(format!("{}.p", definition.name()))
        } else {
            None
        };

        FunctionDefinition::new(
            definition.name(),
            definition
                .arguments()
                .iter()
                .cloned()
                .chain(pointer_name.as_deref().map(|name| {
                    Argument::new(name, types::Pointer::new(definition.result_type().clone()))
                }))
                .collect(),
            if is_result_memory {
                void_type().into()
            } else {
                definition.result_type().clone()
            },
            transform_block(
                definition.body(),
                definition.result_type(),
                pointer_name.as_deref(),
            ),
            definition.options().clone(),
        )
    } else {
        definition.clone()
    }
}

fn transform_block(block: &Block, result_type: &Type, pointer_name: Option<&str>) -> Block {
    let mut instructions = vec![];

    for instruction in block.instructions() {
        instructions.push(transform_instruction(
            instruction,
            result_type,
            pointer_name,
        ));
    }

    if let (TerminalInstruction::Return(return_), Some(pointer_name)) =
        (block.terminal_instruction(), pointer_name)
    {
        Block::new(
            instructions
                .into_iter()
                .chain([Store::new(
                    result_type.clone(),
                    return_.expression().clone(),
                    Variable::new(pointer_name),
                )
                .into()])
                .collect(),
            Return::new(void_type(), void_value()),
        )
    } else {
        Block::new(instructions, block.terminal_instruction().clone())
    }
}

fn transform_instruction(
    instruction: &Instruction,
    result_type: &Type,
    pointer_name: Option<&str>,
) -> Instruction {
    match instruction {
        Instruction::If(if_) => If::new(
            if_.type_().clone(),
            if_.condition().clone(),
            transform_block(if_.then(), result_type, pointer_name),
            transform_block(if_.else_(), result_type, pointer_name),
            if_.name(),
        )
        .into(),
        _ => instruction.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    const WORD_BYTES: usize = 8;

    #[test]
    fn transform_return() {
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
                    "f.p",
                    types::Pointer::new(record_type.clone())
                )],
                void_type(),
                Block::new(
                    vec![Store::new(
                        record_type.clone(),
                        Undefined::new(record_type),
                        Variable::new("f.p")
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
                    "f.p",
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
                                Variable::new("f.p")
                            )
                            .into()],
                            Return::new(void_type(), void_value()),
                        ),
                        Block::new(
                            vec![Store::new(
                                record_type.clone(),
                                Undefined::new(record_type.clone()),
                                Variable::new("f.p")
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
