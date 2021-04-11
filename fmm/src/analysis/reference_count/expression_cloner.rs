use super::utilities;
use crate::build::{self, InstructionBuilder, NameGenerator, TypedExpression};
use crate::ir::*;
use crate::types::{self, Type};
use build::VOID_TYPE;
use std::cell::RefCell;
use std::rc::Rc;

pub struct ExpressionCloner {
    name_generator: Rc<RefCell<NameGenerator>>,
}

impl ExpressionCloner {
    pub fn new(name_generator: Rc<RefCell<NameGenerator>>) -> Self {
        Self { name_generator }
    }

    pub fn clone_expression(&self, builder: &InstructionBuilder, expression: &TypedExpression) {
        match expression.type_() {
            Type::Pointer(_) => {
                utilities::if_heap_pointer(builder, expression, |builder| {
                    builder.atomic_operation(
                        AtomicOperator::Add,
                        utilities::get_counter_pointer(&builder, expression),
                        Primitive::PointerInteger(1),
                    );
                });
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
                );
            }
            Type::Union(_) => unimplemented!(),
            Type::Function(_) | Type::Primitive(_) => {}
        }
    }
}
