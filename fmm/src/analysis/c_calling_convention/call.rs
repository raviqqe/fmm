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
    Ok(Block::new(
        block
            .instructions()
            .iter()
            .map(|instruction| transform_instruction(context, instruction))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect(),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::void_type;
    use pretty_assertions::assert_eq;

    const WORD_BYTES: usize = 8;

    #[test]
    fn transform() {
        let record_type = types::Record::new(vec![
            types::Primitive::Integer64.into(),
            types::Primitive::Integer64.into(),
            types::Primitive::Integer64.into(),
        ]);

        assert_eq!(
            transform_function_definition(
                &Context::new(WORD_BYTES),
                &FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::Integer64,
                    Block::new(
                        vec![
                            Call::new(
                                types::Function::new(
                                    vec![],
                                    record_type.clone(),
                                    types::CallingConvention::Target,
                                ),
                                Variable::new("f"),
                                vec![],
                                "x",
                            )
                            .into(),
                            DeconstructRecord::new(record_type.clone(), Variable::new("x"), 0, "y")
                                .into(),
                        ],
                        Return::new(types::Primitive::Integer64, Variable::new("y")),
                    ),
                    FunctionDefinitionOptions::new()
                        .set_calling_convention(types::CallingConvention::Target),
                )
            ),
            Ok(FunctionDefinition::new(
                "f",
                vec![],
                types::Primitive::Integer64,
                Block::new(
                    vec![
                        AllocateStack::new(record_type.clone(), "x_c_0").into(),
                        Call::new(
                            types::Function::new(
                                vec![types::Pointer::new(record_type.clone()).into()],
                                void_type(),
                                types::CallingConvention::Target
                            ),
                            Variable::new("f"),
                            vec![Variable::new("x_c_0").into()],
                            "x_c_1"
                        )
                        .into(),
                        Load::new(record_type.clone(), Variable::new("x_c_0"), "x").into(),
                        DeconstructRecord::new(record_type, Variable::new("x"), 0, "y").into(),
                    ],
                    Return::new(types::Primitive::Integer64, Variable::new("y")),
                ),
                FunctionDefinitionOptions::new()
                    .set_calling_convention(types::CallingConvention::Target),
            ))
        );
    }
}
