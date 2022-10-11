use super::{context::Context, error::CCallingConventionError, type_};
use crate::{
    build::{self, InstructionBuilder, NameGenerator, TypedExpression},
    ir::*,
    types,
};
use std::rc::Rc;

pub fn transform_function_definition(
    context: &Context,
    definition: &FunctionDefinition,
) -> Result<FunctionDefinition, CCallingConventionError> {
    Ok(FunctionDefinition::new(
        definition.name(),
        definition.arguments().to_vec(),
        definition.result_type().clone(),
        transform_block(context, definition.body())?,
        definition.options().clone(),
    ))
}

fn transform_block(context: &Context, block: &Block) -> Result<Block, CCallingConventionError> {
    let mut instructions = vec![];

    for instruction in block.instructions() {
        instructions.extend(transform_instruction(context, instruction)?);
    }

    Ok(Block::new(
        instructions,
        block.terminal_instruction().clone(),
    ))
}

fn transform_instruction(
    context: &Context,
    instruction: &Instruction,
) -> Result<Vec<Instruction>, CCallingConventionError> {
    Ok(match instruction {
        Instruction::Call(call)
            if call.type_().calling_convention() == types::CallingConvention::Target =>
        {
            match call.function() {
                // TODO Support complex expressions.
                Expression::Variable(variable) => {
                    let builder = InstructionBuilder::new(Rc::new(
                        NameGenerator::new(format!("{}_c_", call.name())).into(),
                    ));
                    let function_type = call.type_();
                    let pointer = builder.allocate_stack(function_type.result().clone());

                    builder.call(
                        build::variable(
                            variable.name(),
                            type_::transform_function(context, function_type),
                        ),
                        call.arguments()
                            .iter()
                            .zip(function_type.arguments())
                            .map(|(argument, type_)| {
                                TypedExpression::new(argument.clone(), type_.clone())
                            })
                            .chain([pointer.clone()])
                            .collect(),
                    )?;

                    builder.add_instruction(Load::new(
                        function_type.result().clone(),
                        pointer.expression().clone(),
                        call.name(),
                    ));

                    builder.into_instructions()
                }
                _ => vec![call.clone().into()],
            }
        }
        _ => vec![instruction.clone()],
    })
}
