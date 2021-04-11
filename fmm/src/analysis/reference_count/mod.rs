mod error;
mod expression_cloner;
mod expression_dropper;
mod expression_mover;
mod global_variable_tag;
mod module_converter;
mod record_clone_function_creator;
mod record_drop_function_creator;
mod utilities;

use self::{
    error::ReferenceCountError, expression_cloner::ExpressionCloner,
    record_clone_function_creator::RecordCloneFunctionCreator,
    record_drop_function_creator::RecordDropFunctionCreator,
};
use crate::{build::NameGenerator, ir::*};
use expression_dropper::ExpressionDropper;
use expression_mover::ExpressionMover;
use module_converter::ModuleConverter;
use std::cell::RefCell;
use std::rc::Rc;

pub fn count_references(module: &Module) -> Result<Module, ReferenceCountError> {
    let name_generator = Rc::new(RefCell::new(NameGenerator::new("rc")));
    let expression_cloner = Rc::new(ExpressionCloner::new());
    let expression_mover = Rc::new(ExpressionMover::new(expression_cloner.clone()));
    let expression_dropper = Rc::new(ExpressionDropper::new());
    let record_clone_function_creator =
        RecordCloneFunctionCreator::new(expression_cloner, name_generator.clone()).into();
    let record_drop_function_creator =
        RecordDropFunctionCreator::new(expression_dropper.clone(), name_generator.clone()).into();

    ModuleConverter::new(
        expression_mover,
        expression_dropper,
        record_clone_function_creator,
        record_drop_function_creator,
        name_generator,
    )
    .convert(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::check_types;
    use crate::types::{self, CallingConvention, Type};

    fn test_transformation(module: &Module) {
        check_types(&count_references(module).unwrap()).unwrap();
    }

    fn test_function_definition(
        arguments: Vec<Argument>,
        body: Block,
        result_type: impl Into<Type>,
    ) {
        test_transformation(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "__test_function",
                arguments,
                body,
                result_type,
                CallingConvention::Target,
                true,
            )],
        ))
    }

    #[test]
    fn transform_empty_module() {
        test_transformation(&Module::new(vec![], vec![], vec![], vec![]));
    }

    #[test]
    fn transform_variable_definition() {
        test_transformation(&Module::new(
            vec![],
            vec![],
            vec![
                VariableDefinition::new(
                    "x",
                    Primitive::PointerInteger(42),
                    types::Primitive::PointerInteger,
                    false,
                    false,
                ),
                VariableDefinition::new(
                    "y",
                    Variable::new("x"),
                    types::Pointer::new(types::Primitive::PointerInteger),
                    false,
                    false,
                ),
            ],
            vec![],
        ));
    }

    #[test]
    fn transform_record_of_one_element() {
        let pointer_type = types::Pointer::new(types::Primitive::Float64);
        let record_type = types::Record::new(vec![
            pointer_type.clone().into(),
            pointer_type.clone().into(),
        ]);

        test_function_definition(
            vec![Argument::new("x", pointer_type)],
            Block::new(
                vec![],
                Return::new(
                    record_type.clone(),
                    Record::new(
                        record_type.clone(),
                        vec![Variable::new("x").into(), Variable::new("x").into()],
                    ),
                ),
            ),
            record_type,
        );
    }
}
