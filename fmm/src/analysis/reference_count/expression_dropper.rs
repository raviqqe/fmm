use super::{error::ReferenceCountError, utilities};
use crate::{
    build::{self, InstructionBuilder, NameGenerator, TypedExpression, VOID_TYPE, VOID_VALUE},
    ir::*,
    types::{self, Type},
};
use std::{cell::RefCell, rc::Rc};

pub struct ExpressionDropper {
    name_generator: Rc<RefCell<NameGenerator>>,
}

impl ExpressionDropper {
    pub fn new(name_generator: Rc<RefCell<NameGenerator>>) -> Self {
        Self { name_generator }
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
                                utilities::get_counter_pointer(&builder, expression),
                                Primitive::PointerInteger(1),
                            ),
                            Primitive::PointerInteger(0),
                        ),
                        |builder| {
                            self.drop_expression(&builder, &builder.load(expression.clone()));
                            builder.free_heap(expression.clone());

                            builder.branch(VOID_VALUE.clone())
                        },
                        |builder| builder.branch(VOID_VALUE.clone()),
                    );
                });
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
                );
            }
            Type::Union(_) => Err(ReferenceCountError::UnionNotSupported)?,
            Type::Function(_) | Type::Primitive(_) => {}
        }

        Ok(())
    }
}
