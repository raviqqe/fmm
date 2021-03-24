use crate::ir::*;
use crate::types::Type;
use std::collections::HashMap;

pub fn get_expression_type(expression: &Expression, variables: &HashMap<String, Type>) -> Type {
    match expression {
        Expression::AlignOf(_) => AlignOf::RESULT_TYPE.into(),
        Expression::BitCast(bit_cast) => bit_cast.to().clone(),
        Expression::Primitive(primitive) => primitive.type_().into(),
        Expression::Record(record) => record.type_().clone().into(),
        Expression::SizeOf(_) => SizeOf::RESULT_TYPE.into(),
        Expression::Undefined(undefined) => undefined.type_().clone(),
        Expression::Union(union) => union.type_().clone().into(),
        Expression::Variable(variable) => variables[variable.name()].clone(),
    }
}
