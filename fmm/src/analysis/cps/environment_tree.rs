use super::free_variable;
use crate::{ir::*, types::Type};
use fnv::FnvHashMap;
use std::rc::Rc;

#[derive(Debug)]
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
                    let tree = tree.map(Rc::new);

                    Some(EnvironmentTree::If(
                        build_conditional_block(if_.then(), &tree, local_variables),
                        build_conditional_block(if_.else_(), &tree, local_variables),
                    ))
                }
                _ => tree,
            }
        }
    }
}

fn build_conditional_block<'a>(
    block: &'a Block,
    tree: &Option<Rc<EnvironmentTree<'a>>>,
    local_variables: &'a FnvHashMap<String, Type>,
) -> Option<Rc<EnvironmentTree<'a>>> {
    if block.terminal_instruction().is_branch() {
        tree.clone()
    } else {
        build(block, local_variables).map(Into::into)
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
