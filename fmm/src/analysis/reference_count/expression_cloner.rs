use super::error::ReferenceCountError;
use super::utilities;
use crate::build::{self, InstructionBuilder, TypedExpression};
use crate::ir::*;
use crate::types::{self, Type};
use build::VOID_TYPE;

pub struct ExpressionCloner {}

impl ExpressionCloner {
    pub fn new() -> Self {
        Self {}
    }

    pub fn clone_expression(
        &self,
        builder: &InstructionBuilder,
        expression: &TypedExpression,
    ) -> Result<(), ReferenceCountError> {
        match expression.type_() {
            Type::Pointer(_) => {
                utilities::if_heap_pointer(builder, expression, |builder| {
                    builder.atomic_operation(
                        AtomicOperator::Add,
                        utilities::get_counter_pointer(&builder, expression)?,
                        Primitive::PointerInteger(1),
                    )?;

                    Ok(())
                })?;
            }
            Type::Record(record_type) => {
                builder.call(
                    build::variable(
                        utilities::get_record_clone_function_name(record_type),
                        types::Function::new(
                            vec![record_type.clone().into()],
                            VOID_TYPE.clone(),
                            types::CallingConvention::Target,
                        ),
                    ),
                    vec![expression.clone()],
                )?;
            }
            Type::Union(_) => Err(ReferenceCountError::UnionNotSupported)?,
            Type::Function(_) | Type::Primitive(_) => {}
        }

        Ok(())
    }
}
