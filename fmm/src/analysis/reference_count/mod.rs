mod expression_converter;
mod global_variable_tagger;
mod module_converter;
mod variable_lifetime_manager;

use crate::{build::NameGenerator, ir::*};
use expression_converter::ExpressionConverter;
use module_converter::ModuleConverter;
use std::cell::RefCell;
use std::rc::Rc;

use self::variable_lifetime_manager::VariableLifetimeManger;

pub fn count_references(module: &Module) -> Module {
    let name_generator = Rc::new(RefCell::new(NameGenerator::new("rc")));
    let variable_lifetime_manager = VariableLifetimeManger::new(name_generator).into();
    let expression_converter = Rc::new(ExpressionConverter::new(variable_lifetime_manager)).into();

    ModuleConverter::new(expression_converter).convert(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::check_types;
    use crate::types::{self, CallingConvention, Type};

    fn test_transformation(module: &Module) {
        check_types(&count_references(module)).unwrap();
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
