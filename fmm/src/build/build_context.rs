use crate::ir::*;
use crate::types::{self, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct BuildContext {
    instructions: Vec<Instruction>,
    expression: Expression,
    type_: Type,
}

impl BuildContext {
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

    pub fn from_expression(expression: impl Into<Expression>, type_: impl Into<Type>) -> Self {
        Self::new(vec![], expression, type_)
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

impl From<Primitive> for BuildContext {
    fn from(primitive: Primitive) -> Self {
        Self::from_expression(
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

impl From<Record> for BuildContext {
    fn from(record: Record) -> Self {
        Self::from_expression(record.clone(), record.type_().clone())
    }
}

impl From<Undefined> for BuildContext {
    fn from(undefined: Undefined) -> Self {
        Self::from_expression(undefined.clone(), undefined.type_().clone())
    }
}

impl From<Union> for BuildContext {
    fn from(union: Union) -> Self {
        Self::from_expression(union.clone(), union.type_().clone())
    }
}
