use super::{context::Context, utilities};
use crate::{
    ir::*,
    types::{self, void_type, Type},
};

pub fn transform(context: &Context, definition: &FunctionDefinition) -> Option<FunctionDefinition> {
    if definition.type_().calling_convention() == types::CallingConvention::Target
        && utilities::is_memory_class(context, definition.result_type())
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
        block.clone()
    }
}
