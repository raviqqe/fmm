use crate::ir::*;
use crate::types::{self, Type};

// Do not derive Clone to guarantee that it's consumed only once!
pub struct InstructionedExpression {
    instructions: Vec<Instruction>,
    expression: Expression,
    type_: Type,
}

impl InstructionedExpression {
    pub fn new(
        instructions: impl IntoIterator<Item = Instruction>,
        expression: impl Into<Expression>,
        type_: impl Into<Type>,
    ) -> Self {
        Self {
            instructions: instructions.into_iter().collect(),
            expression: expression.into(),
            type_: type_.into(),
        }
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }
}

impl From<Primitive> for InstructionedExpression {
    fn from(primitive: Primitive) -> Self {
        Self::new(
            vec![],
            primitive,
            match primitive {
                Primitive::Bool(_) => types::Primitive::Bool,
                Primitive::Float32(_) => types::Primitive::Float32,
                Primitive::Float64(_) => types::Primitive::Float64,
                Primitive::Integer8(_) => types::Primitive::Integer8,
                Primitive::Integer32(_) => types::Primitive::Integer32,
                Primitive::Integer64(_) => types::Primitive::Integer64,
                Primitive::PointerInteger(_) => types::Primitive::PointerInteger,
            },
        )
    }
}

impl From<Record> for InstructionedExpression {
    fn from(record: Record) -> Self {
        Self::new(vec![], record.clone(), record.type_().clone())
    }
}

impl From<Undefined> for InstructionedExpression {
    fn from(undefined: Undefined) -> Self {
        Self::new(vec![], undefined.clone(), undefined.type_().clone())
    }
}

impl From<Union> for InstructionedExpression {
    fn from(union: Union) -> Self {
        Self::new(vec![], union.clone(), union.type_().clone())
    }
}
