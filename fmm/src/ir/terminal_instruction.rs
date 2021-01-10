use super::branch::Branch;
use super::return_::Return;

#[derive(Clone, Debug, PartialEq)]
pub enum TerminalInstruction {
    Branch(Branch),
    Return(Return),
    Unreachable,
}

impl TerminalInstruction {
    pub fn to_branch(&self) -> Option<&Branch> {
        if let TerminalInstruction::Branch(branch) = self {
            Some(branch)
        } else {
            None
        }
    }

    pub fn to_return(&self) -> Option<&Branch> {
        if let TerminalInstruction::Branch(return_) = self {
            Some(return_)
        } else {
            None
        }
    }
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
