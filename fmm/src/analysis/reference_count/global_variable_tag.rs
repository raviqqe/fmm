use super::error::ReferenceCountError;
use crate::ir::*;
use crate::types::{self, Type};
use std::collections::HashMap;

pub fn tag_expression(
    expression: &Expression,
    type_: &Type,
    global_variables: &HashMap<String, Type>,
) -> Result<Expression, ReferenceCountError> {
    let tag_expression = |expression, type_| tag_expression(expression, type_, global_variables);

    Ok(match expression {
        Expression::BitCast(bit_cast) => BitCast::new(
            bit_cast.from().clone(),
            bit_cast.to().clone(),
            tag_expression(bit_cast.expression(), bit_cast.from())?,
        )
        .into(),
        Expression::BitwiseNotOperation(operation) => BitwiseNotOperation::new(
            operation.type_(),
            tag_expression(operation.value(), &operation.type_().into())?,
        )
        .into(),
        Expression::BitwiseOperation(operation) => BitwiseOperation::new(
            operation.type_(),
            operation.operator(),
            tag_expression(operation.lhs(), &operation.type_().into())?,
            tag_expression(operation.rhs(), &operation.type_().into())?,
        )
        .into(),
        Expression::Record(record) => Record::new(
            record.type_().clone(),
            record
                .elements()
                .iter()
                .zip(record.type_().elements())
                .map(|(element, type_)| tag_expression(element, type_))
                .collect::<Result<_, _>>()?,
        )
        .into(),
        Expression::Union(_) => return Err(ReferenceCountError::UnionNotSupported),
        Expression::Variable(variable) => {
            if global_variables.contains_key(variable.name()) {
                tag_pointer_to_global_variable(variable, type_)
            } else {
                variable.clone().into()
            }
        }
        Expression::AlignOf(_)
        | Expression::Primitive(_)
        | Expression::SizeOf(_)
        | Expression::Undefined(_) => expression.clone(),
    })
}

pub fn untag_pointer(expression: &Expression, type_: &Type) -> Expression {
    BitCast::new(
        types::Primitive::PointerInteger,
        type_.clone(),
        BitwiseOperation::new(
            types::Primitive::PointerInteger,
            BitwiseOperator::And,
            BitCast::new(
                type_.clone(),
                types::Primitive::PointerInteger,
                expression.clone(),
            ),
            BitwiseNotOperation::new(
                types::Primitive::PointerInteger,
                Primitive::PointerInteger(1),
            ),
        ),
    )
    .into()
}

fn tag_pointer_to_global_variable(variable: &Variable, type_: &Type) -> Expression {
    BitCast::new(
        types::Primitive::PointerInteger,
        type_.clone(),
        BitwiseOperation::new(
            types::Primitive::PointerInteger,
            BitwiseOperator::Or,
            BitCast::new(
                type_.clone(),
                types::Primitive::PointerInteger,
                variable.clone(),
            ),
            Primitive::PointerInteger(1),
        ),
    )
    .into()
}
