use super::error::CpsTransformationError;
use crate::{ir::*, types::CallingConvention};

pub fn validate_target_function_definition(
    definition: &FunctionDefinition,
) -> Result<(), CpsTransformationError> {
    validate_block(definition.body())
}

fn validate_block(block: &Block) -> Result<(), CpsTransformationError> {
    for instruction in block.instructions() {
        validate_instruction(instruction)?;
    }

    Ok(())
}

fn validate_instruction(instruction: &Instruction) -> Result<(), CpsTransformationError> {
    match instruction {
        Instruction::Call(call) => {
            if call.type_().calling_convention() == CallingConvention::Source {
                Err(CpsTransformationError::InvalidCallingConvention(
                    call.clone(),
                ))
            } else {
                Ok(())
            }
        }
        Instruction::If(if_) => {
            validate_block(if_.then())?;
            validate_block(if_.else_())
        }
        _ => Ok(()),
    }
}
