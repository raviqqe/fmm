use crate::{ir::Instruction, types::Type};

pub enum EnvironmentTree<'a> {
    Call(Vec<(&'a str, &'a Type)>),
    If(Box<EnvironmentTree<'a>>, Box<EnvironmentTree<'a>>),
}

impl<'a> EnvironmentTree<'a> {
    pub fn new(instructions: &'a [Instruction]) -> Self {
        todo!()
    }
}
