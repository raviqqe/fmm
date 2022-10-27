use super::free_variable;
use crate::{
    analysis::local_variable,
    ir::*,
    types::{CallingConvention, Type},
};
use fnv::FnvHashMap;
use indexmap::IndexSet;

pub fn transform(module: &mut Module) {
    for definition in module.function_definitions_mut() {
        transform_function_definition(definition);
    }
}

fn transform_function_definition(definition: &mut FunctionDefinition) {
    if definition.type_().calling_convention() != CallingConvention::Source {
        return;
    }

    let local_variables = local_variable::collect(definition)
        .into_iter()
        .map(|(name, type_)| (name.into(), type_))
        .collect();

    transform_block(definition.body_mut(), &local_variables);
}

fn transform_block(block: &mut Block, local_variables: &FnvHashMap<String, Type>) {
    let terminal_instruction = block.terminal_instruction().clone();

    for index in (0..block.instructions().len()).rev() {
        match block.instructions_mut().split_at_mut(index) {
            ([.., Instruction::Call(call)], instructions)
                if call.type_().calling_convention() == CallingConvention::Source =>
            {
                *call.environment_mut() = Some(create_environment(
                    &call,
                    &instructions,
                    &terminal_instruction,
                ));

                return;
            }
            ([.., Instruction::If(if_)], _) => {
                transform_block(if_.then_mut(), local_variables);
                transform_block(if_.else_mut(), local_variables);
            }
            _ => {}
        }
    }
}

fn create_environment(
    call: &Call,
    instructions: &[Instruction],
    terminal_instruction: &TerminalInstruction,
) -> IndexSet<String> {
    free_variable::collect(instructions, terminal_instruction)
        .into_iter()
        .filter(|name| *name != call.name())
        .map(|name| name.into())
        .collect()
}
