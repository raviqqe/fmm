use crate::ir::*;
use crate::types::{self, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct TypedExpression {
    expression: Expression,
    type_: Type,
}

impl TypedExpression {
    pub fn new(expression: impl Into<Expression>, type_: impl Into<Type>) -> Self {
        Self {
            expression: expression.into(),
            type_: type_.into(),
        }
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }

    pub fn type_(&self) -> &Type {
        &self.type_
    }
}

impl From<AlignOf> for TypedExpression {
    fn from(align_of: AlignOf) -> Self {
        Self::new(align_of.clone(), align_of.type_().clone())
    }
}

impl From<BitCast> for TypedExpression {
    fn from(bit_cast: BitCast) -> Self {
        Self::new(bit_cast.clone(), bit_cast.to().clone())
    }
}

impl From<Primitive> for TypedExpression {
    fn from(primitive: Primitive) -> Self {
        Self::new(
            primitive,
            match primitive {
                Primitive::Boolean(_) => types::Primitive::Boolean,
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

impl From<Record> for TypedExpression {
    fn from(record: Record) -> Self {
        Self::new(record.clone(), record.type_().clone())
    }
}

impl From<SizeOf> for TypedExpression {
    fn from(size_of: SizeOf) -> Self {
        Self::new(size_of.clone(), size_of.type_().clone())
    }
}

impl From<Undefined> for TypedExpression {
    fn from(undefined: Undefined) -> Self {
        Self::new(undefined.clone(), undefined.type_().clone())
    }
}

impl From<Union> for TypedExpression {
    fn from(union: Union) -> Self {
        Self::new(union.clone(), union.type_().clone())
    }
}
