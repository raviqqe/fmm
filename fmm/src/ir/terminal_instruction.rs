use super::branch::Branch;
use super::return_::Return;

#[derive(Clone, Debug, PartialEq)]
pub enum TerminalInstruction {
    Branch(Branch),
    Return(Return),
    Unreachable,
}

impl From<Branch> for TerminalInstruction {
    fn from(branch: Branch) -> Self {
        Self::Branch(branch)
    }
}

impl From<Return> for TerminalInstruction {
    fn from(return_: Return) -> Self {
        Self::Return(return_)
    }
}
