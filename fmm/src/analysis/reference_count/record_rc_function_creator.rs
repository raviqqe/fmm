use super::{expression_lifetime_manager::ExpressionLifetimeManger, utilities};
use crate::{
    build::{self, InstructionBuilder, NameGenerator, TypedExpression},
    ir::*,
    types::{self, CallingConvention},
};
use build::{VOID_TYPE, VOID_VALUE};
use std::cell::RefCell;
use std::rc::Rc;

const ARGUMENT_NAME: &str = "x";

pub struct RecordRcFunctionCreator {
    expression_lifetime_manager: Rc<ExpressionLifetimeManger>,
    name_generator: Rc<RefCell<NameGenerator>>,
}

impl RecordRcFunctionCreator {
    pub fn new(
        expression_lifetime_manager: Rc<ExpressionLifetimeManger>,
        name_generator: Rc<RefCell<NameGenerator>>,
    ) -> Self {
        Self {
            expression_lifetime_manager,
            name_generator,
        }
    }

    pub fn create_record_clone_function(&self, record_type: &types::Record) -> FunctionDefinition {
        let builder = InstructionBuilder::new(self.name_generator.clone());
        let elements = self.extract_elements(&builder, &Variable::new(ARGUMENT_NAME), record_type);

        FunctionDefinition::new(
            utilities::get_record_clone_function_name(record_type),
            vec![Argument::new(ARGUMENT_NAME, record_type.clone())],
            Block::new(
                builder
                    .into_instructions()
                    .into_iter()
                    .chain(elements.into_iter().flat_map(|element| {
                        self.expression_lifetime_manager
                            .clone_expression(element.expression(), element.type_())
                    }))
                    .collect(),
                Return::new(VOID_TYPE.clone(), VOID_VALUE.clone()),
            ),
            VOID_TYPE.clone(),
            CallingConvention::Target,
            false,
        )
    }

    pub fn create_record_drop_function(&self, record_type: &types::Record) -> FunctionDefinition {
        let builder = InstructionBuilder::new(self.name_generator.clone());
        let elements = self.extract_elements(&builder, &Variable::new(ARGUMENT_NAME), record_type);

        FunctionDefinition::new(
            utilities::get_record_drop_function_name(record_type),
            vec![Argument::new(ARGUMENT_NAME, record_type.clone())],
            Block::new(
                builder
                    .into_instructions()
                    .into_iter()
                    .chain(elements.into_iter().flat_map(|element| {
                        self.expression_lifetime_manager
                            .drop_expression(element.expression(), element.type_())
                    }))
                    .collect(),
                Return::new(VOID_TYPE.clone(), VOID_VALUE.clone()),
            ),
            VOID_TYPE.clone(),
            CallingConvention::Target,
            false,
        )
    }

    fn extract_elements(
        &self,
        builder: &InstructionBuilder,
        variable: &Variable,
        record_type: &types::Record,
    ) -> Vec<TypedExpression> {
        record_type
            .elements()
            .iter()
            .enumerate()
            .map(|(index, _)| {
                builder.deconstruct_record(
                    build::variable(variable.name(), record_type.clone()),
                    index,
                )
            })
            .collect()
    }
}
