use crate::ir::*;
use crate::types::Type;

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
        Self::new(align_of, AlignOf::RESULT_TYPE)
    }
}

impl From<BitCast> for TypedExpression {
    fn from(bit_cast: BitCast) -> Self {
        Self::new(bit_cast.clone(), bit_cast.to().clone())
    }
}

impl From<BitwiseOperation> for TypedExpression {
    fn from(operation: BitwiseOperation) -> Self {
        Self::new(operation.clone(), operation.type_())
    }
}

impl From<Primitive> for TypedExpression {
    fn from(primitive: Primitive) -> Self {
        Self::new(primitive, primitive.type_())
    }
}

impl From<Record> for TypedExpression {
    fn from(record: Record) -> Self {
        Self::new(record.clone(), record.type_().clone())
    }
}

impl From<SizeOf> for TypedExpression {
    fn from(size_of: SizeOf) -> Self {
        Self::new(size_of, SizeOf::RESULT_TYPE)
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
