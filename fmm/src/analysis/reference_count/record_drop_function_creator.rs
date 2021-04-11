use super::error::ReferenceCountError;
use super::expression_dropper::ExpressionDropper;
use super::utilities;
use crate::{
    build::{self, InstructionBuilder, NameGenerator},
    ir::*,
    types::{self, CallingConvention},
};
use build::{VOID_TYPE, VOID_VALUE};
use std::cell::RefCell;
use std::rc::Rc;

const ARGUMENT_NAME: &str = "x";

pub struct RecordDropFunctionCreator {
    expression_dropper: Rc<ExpressionDropper>,
    name_generator: Rc<RefCell<NameGenerator>>,
}

impl RecordDropFunctionCreator {
    pub fn new(
        expression_dropper: Rc<ExpressionDropper>,
        name_generator: Rc<RefCell<NameGenerator>>,
    ) -> Self {
        Self {
            expression_dropper,
            name_generator,
        }
    }

    pub fn create(
        &self,
        record_type: &types::Record,
    ) -> Result<FunctionDefinition, ReferenceCountError> {
        let builder = InstructionBuilder::new(self.name_generator.clone());

        for element in utilities::extract_record_elements(
            &builder,
            &Variable::new(ARGUMENT_NAME),
            record_type,
        )? {
            self.expression_dropper
                .drop_expression(&builder, &element)?;
        }

        Ok(FunctionDefinition::new(
            utilities::get_record_drop_function_name(record_type),
            vec![Argument::new(ARGUMENT_NAME, record_type.clone())],
            builder.return_(VOID_VALUE.clone()),
            VOID_TYPE.clone(),
            CallingConvention::Target,
            false,
        ))
    }
}
