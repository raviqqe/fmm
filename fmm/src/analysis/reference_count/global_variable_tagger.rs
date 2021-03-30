use crate::ir::*;
use crate::types::{self, Type};
use std::collections::HashSet;

pub fn tag_expression(
    expression: &Expression,
    type_: &Type,
    global_variables: &HashSet<String>,
) -> Expression {
    match expression {
        Expression::BitCast(bit_cast) => {
            tag_expression(bit_cast.expression(), bit_cast.from(), global_variables)
        }
        Expression::Record(record) => Record::new(
            record.type_().clone(),
            record
                .elements()
                .iter()
                .zip(record.type_().elements())
                .map(|(element, type_)| tag_expression(element, type_, global_variables))
                .collect(),
        )
        .into(),
        Expression::Union(_) => todo!(),
        Expression::Variable(variable) => {
            if global_variables.contains(variable.name()) {
                tag_variable(variable, type_).into()
            } else {
                variable.clone().into()
            }
        }
        Expression::AlignOf(_)
        | Expression::Primitive(_)
        | Expression::SizeOf(_)
        | Expression::Undefined(_) => expression.clone(),
    }
}

fn tag_variable(variable: &Variable, type_: &Type) -> BitOperation {
    BitOperation::new(
        BitOperator::Or,
        BitCast::new(
            type_.clone(),
            types::Primitive::PointerInteger,
            variable.clone(),
        ),
        Primitive::PointerInteger(1),
    )
}
