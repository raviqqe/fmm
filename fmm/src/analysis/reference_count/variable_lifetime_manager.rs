use crate::build::{self, InstructionBuilder, NameGenerator, TypedExpression};
use crate::ir::*;
use crate::types::{self, Type};
use once_cell::sync::Lazy;
use std::cell::RefCell;
use std::rc::Rc;

static VOID_TYPE: Lazy<types::Record> = Lazy::new(|| types::Record::new(vec![]));
static VOID_VALUE: Lazy<Record> = Lazy::new(|| Record::new(VOID_TYPE.clone(), vec![]));

pub struct VariableLifetimeManger {
    name_generator: Rc<RefCell<NameGenerator>>,
}

impl VariableLifetimeManger {
    pub fn new(name_generator: Rc<RefCell<NameGenerator>>) -> Self {
        Self { name_generator }
    }

    pub fn clone_variable(&self, variable: &Variable, type_: &Type) -> Vec<Instruction> {
        let builder = InstructionBuilder::new(self.name_generator.clone());

        self.clone_typed_expression(&builder, &build::variable(variable.name(), type_.clone()));

        builder.into_instructions()
    }

    fn clone_typed_expression(&self, builder: &InstructionBuilder, expression: &TypedExpression) {
        match expression.type_() {
            Type::Pointer(_) => {
                // TODO Handle pointers to static variables.
                builder.atomic_operation(
                    AtomicOperator::Add,
                    self.get_counter_pointer(&builder, expression),
                    Primitive::PointerInteger(1),
                );
            }
            Type::Record(_) => todo!(),
            Type::Union(_) => todo!(),
            Type::Function(_) | Type::Primitive(_) => {}
        }
    }

    pub fn drop_variable(&self, variable: &Variable, type_: &Type) -> Vec<Instruction> {
        let builder = InstructionBuilder::new(self.name_generator.clone());

        self.drop_typed_expression(&builder, &build::variable(variable.name(), type_.clone()));

        builder.into_instructions()
    }

    fn drop_typed_expression(&self, builder: &InstructionBuilder, expression: &TypedExpression) {
        match expression.type_() {
            Type::Pointer(_) => {
                // TODO Handle pointers to static variables.
                let old_count = builder.atomic_operation(
                    AtomicOperator::Subtract,
                    self.get_counter_pointer(&builder, expression),
                    Primitive::PointerInteger(1),
                );

                builder.if_(
                    builder.comparison_operation(
                        ComparisonOperator::Equal,
                        old_count,
                        Primitive::PointerInteger(0),
                    ),
                    |builder| {
                        self.drop_typed_expression(&builder, &builder.load(expression.clone()));

                        builder.free_heap(expression.clone());

                        builder.branch(VOID_VALUE.clone())
                    },
                    |builder| builder.branch(VOID_VALUE.clone()),
                );
            }
            Type::Record(_) => todo!(),
            Type::Union(_) => todo!(),
            Type::Function(_) | Type::Primitive(_) => {}
        }
    }

    fn get_counter_pointer(
        &self,
        builder: &InstructionBuilder,
        expression: &TypedExpression,
    ) -> TypedExpression {
        builder.pointer_address(
            build::bit_cast(
                types::Pointer::new(types::Primitive::PointerInteger),
                expression.clone(),
            ),
            Primitive::PointerInteger(-1),
        )
    }
}
