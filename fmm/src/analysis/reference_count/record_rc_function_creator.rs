use super::expression_dropper::ExpressionDropper;
use super::{expression_cloner::ExpressionCloner, utilities};
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
    expression_cloner: Rc<ExpressionCloner>,
    expression_dropper: Rc<ExpressionDropper>,
    name_generator: Rc<RefCell<NameGenerator>>,
}

impl RecordRcFunctionCreator {
    pub fn new(
        expression_cloner: Rc<ExpressionCloner>,
        expression_dropper: Rc<ExpressionDropper>,
        name_generator: Rc<RefCell<NameGenerator>>,
    ) -> Self {
        Self {
            expression_cloner,
            expression_dropper,
            name_generator,
        }
    }

    pub fn create_record_clone_function(&self, record_type: &types::Record) -> FunctionDefinition {
        let builder = InstructionBuilder::new(self.name_generator.clone());

        for element in self.extract_elements(&builder, &Variable::new(ARGUMENT_NAME), record_type) {
            self.expression_cloner
                .clone_expression(&builder, &element);
        }

        FunctionDefinition::new(
            utilities::get_record_clone_function_name(record_type),
            vec![Argument::new(ARGUMENT_NAME, record_type.clone())],
            builder.return_(VOID_VALUE.clone()),
            VOID_TYPE.clone(),
            CallingConvention::Target,
            false,
        )
    }

    pub fn create_record_drop_function(&self, record_type: &types::Record) -> FunctionDefinition {
        let builder = InstructionBuilder::new(self.name_generator.clone());

        for element in self.extract_elements(&builder, &Variable::new(ARGUMENT_NAME), record_type) {
            self.expression_dropper.drop_expression(&builder, &element);
        }

        FunctionDefinition::new(
            utilities::get_record_drop_function_name(record_type),
            vec![Argument::new(ARGUMENT_NAME, record_type.clone())],
            builder.return_(VOID_VALUE.clone()),
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
