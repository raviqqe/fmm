use super::utilities::get_expression_type;
use crate::ir::*;
use crate::types::{self, Type};
use std::collections::HashMap;

pub fn add_expression_header(expression: &Expression, variables: &HashMap<String, Type>) -> Record {
    Record::new(
        add_type_header(&get_expression_type(expression, variables)),
        vec![calculate_expression_header(&expression), expression.clone()],
    )
    .into()
}

pub fn calculate_expression_header(
    expression: &Expression,
    variables: &HashMap<String, Type>,
) -> (Vec<Instruction>, Expression) {
    match expression {
        Expression::BitCast(bit_cast) => (vec![], calculate_type_header(bit_cast.to())),
        Expression::Record(record) => (
            vec![],
            calculate_type_header(&record.type_().clone().into()),
        ),
        Expression::Union(union) => todo!(),
        Expression::Variable(variable) => match &variables[variable.name()] {
            Type::Union(union) => {
                DeconstructRecord::new(add_type_header(union.clone()), 0, variable.clone().into())
                    .into()
            }
            type_ => calculate_type_header(type_),
        },
        Expression::AlignOf(_)
        | Expression::Primitive(_)
        | Expression::SizeOf(_)
        | Expression::Undefined(_) => Primitive::Integer64(0).into(),
    }
}

fn calculate_type_header(type_: &Type) -> Expression {
    match type_ {
        Type::Pointer(_) => Primitive::Integer64(1).into(),
        Type::Record(_) => todo!(),
        Type::Union(_) => unreachable!(),
        Type::Function(_) | Type::Primitive(_) => Primitive::Integer64(0).into(),
    }
}

fn calculate_union_header(union: &Union) -> Expression {
    calculate_type_header(&union.type_().members()[union.member_index()])
}

pub fn add_type_header(type_: &Type) -> types::Record {
    types::Record::new(vec![types::Primitive::Integer64.into(), type_.clone()])
}
