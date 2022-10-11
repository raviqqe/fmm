use super::{context::Context, type_};
use crate::{
    ir::*,
    types::{self, void_type, Type},
};

pub fn transform(context: &Context, definition: &FunctionDefinition) -> Option<FunctionDefinition> {
    if definition.type_().calling_convention() == types::CallingConvention::Target
        && type_::is_memory_class(context, definition.result_type())
    {
        let pointer_name = format!("{}.p", definition.name());

        Some(FunctionDefinition::new(
            definition.name(),
            definition
                .arguments()
                .iter()
                .cloned()
                .chain([Argument::new(
                    &pointer_name,
                    types::Pointer::new(definition.result_type().clone()),
                )])
                .collect(),
            void_type(),
            transform_block(definition.body(), definition.result_type(), &pointer_name),
            definition.options().clone(),
        ))
    } else {
        None
    }
}

fn transform_block(block: &Block, result_type: &Type, pointer_name: &str) -> Block {
    let mut instructions = vec![];

    for instruction in block.instructions() {
        instructions.push(transform_instruction(
            instruction,
            result_type,
            pointer_name,
        ));
    }

    if let TerminalInstruction::Return(return_) = block.terminal_instruction() {
        Block::new(
            block
                .instructions()
                .iter()
                .cloned()
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
    pointer_name: &str,
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
            Some(FunctionDefinition::new(
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
            ))
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
            Some(FunctionDefinition::new(
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
            ))
        );
    }
}
