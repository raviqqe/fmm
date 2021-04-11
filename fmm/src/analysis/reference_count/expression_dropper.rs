use super::{error::ReferenceCountError, utilities};
use crate::{
    build::{self, InstructionBuilder, TypedExpression, VOID_TYPE, VOID_VALUE},
    ir::*,
    types::{self, Type},
};

pub struct ExpressionDropper {}

impl ExpressionDropper {
    pub fn new() -> Self {
        Self {}
    }

    pub fn drop_expression(
        &self,
        builder: &InstructionBuilder,
        expression: &TypedExpression,
    ) -> Result<(), ReferenceCountError> {
        match expression.type_() {
            Type::Pointer(_) => {
                utilities::if_heap_pointer(builder, expression, |builder| {
                    builder.if_(
                        builder.comparison_operation(
                            ComparisonOperator::Equal,
                            builder.atomic_operation(
                                AtomicOperator::Subtract,
                                utilities::get_counter_pointer(&builder, expression)?,
                                Primitive::PointerInteger(1),
                            )?,
                            Primitive::PointerInteger(0),
                        )?,
                        |builder| -> Result<_, ReferenceCountError> {
                            self.drop_expression(&builder, &builder.load(expression.clone())?)?;
                            builder.free_heap(expression.clone())?;

                            Ok(builder.branch(VOID_VALUE.clone()))
                        },
                        |builder| Ok(builder.branch(VOID_VALUE.clone())),
                    )?;

                    Ok(())
                })?;
            }
            Type::Record(record_type) => {
                builder.call(
                    build::variable(
                        utilities::get_record_drop_function_name(record_type),
                        types::Function::new(
                            vec![record_type.clone().into()],
                            VOID_TYPE.clone(),
                            types::CallingConvention::Target,
                        ),
                    ),
                    vec![expression.clone()],
                )?;
            }
            Type::Union(_) => return Err(ReferenceCountError::UnionNotSupported),
            Type::Function(_) | Type::Primitive(_) => {}
        }

        Ok(())
    }
}
