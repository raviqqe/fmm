use super::free_variable;
use crate::{ir::*, types::Type};
use fnv::FnvHashMap;
use std::rc::Rc;

pub enum EnvironmentTree<'a> {
    Call(Vec<(&'a str, &'a Type)>, Option<Box<EnvironmentTree<'a>>>),
    If(
        Option<Rc<EnvironmentTree<'a>>>,
        Option<Rc<EnvironmentTree<'a>>>,
    ),
}

pub fn build<'a>(
    block: &'a Block,
    local_variables: &'a FnvHashMap<String, Type>,
) -> Option<EnvironmentTree<'a>> {
    build_from_instructions(
        block.instructions(),
        block.terminal_instruction(),
        local_variables,
    )
}

fn build_from_instructions<'a>(
    instructions: &'a [Instruction],
    terminal_instruction: &'a TerminalInstruction,
    local_variables: &'a FnvHashMap<String, Type>,
) -> Option<EnvironmentTree<'a>> {
    match &instructions {
        [] => None,
        [instruction, ..] => {
            let tree = build_from_instructions(instructions, terminal_instruction, local_variables);

            match instruction {
                Instruction::Call(call) => Some(EnvironmentTree::Call(
                    create_continuation_environment(
                        call,
                        instructions,
                        terminal_instruction,
                        local_variables,
                    ),
                    tree.map(Into::into),
                )),
                Instruction::If(if_) => {
                    let tree = tree.map(Into::into);

                    Some(EnvironmentTree::If(
                        if if_.then().terminal_instruction().is_branch() {
                            tree.clone()
                        } else {
                            build(if_.then(), local_variables).map(Into::into)
                        },
                        if if_.else_().terminal_instruction().is_branch() {
                            tree.clone()
                        } else {
                            build(if_.else_(), local_variables).map(Into::into)
                        },
                    ))
                }
                _ => tree,
            }
        }
    }
}

fn create_continuation_environment<'a>(
    call: &Call,
    instructions: &'a [Instruction],
    terminal_instruction: &'a TerminalInstruction,
    local_variables: &'a FnvHashMap<String, Type>,
) -> Vec<(&'a str, &'a Type)> {
    free_variable::collect(instructions, terminal_instruction)
        .into_iter()
        .filter(|name| *name != call.name())
        .flat_map(|name| {
            local_variables
                .get_key_value(name)
                .map(|(name, type_)| (name.as_str(), type_))
        })
        .collect()
}
