use super::block_state::BlockState;
use super::names::*;
use super::typed_expression::*;
use crate::ir::*;
use crate::types::{self, Type};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ModuleState {
    function_definitions: Rc<RefCell<Vec<FunctionDefinition>>>,
}

impl ModuleState {
    pub fn new() -> Self {
        Self::from_function_definitions(RefCell::new(vec![]).into())
    }

    fn from_function_definitions(
        function_definitions: Rc<RefCell<Vec<FunctionDefinition>>>,
    ) -> Self {
        Self {
            function_definitions,
        }
    }

    pub fn define_function(
        &self,
        name: Option<impl Into<String>>,
        arguments: Vec<Argument>,
        body: impl Fn(BlockState) -> Block,
        result_type: impl Into<Type>,
    ) -> TypedExpression {
        let result_type = result_type.into();
        let name = name.map(|name| name.into()).unwrap_or_else(generate_name);

        self.function_definitions
            .borrow_mut()
            .push(FunctionDefinition::new(
                &name,
                arguments.clone(),
                body(BlockState::new(Self::from_function_definitions(
                    self.function_definitions.clone(),
                ))),
                result_type.clone(),
            ));

        TypedExpression::new(
            Variable::new(name),
            types::Function::new(
                arguments
                    .iter()
                    .map(|argument| argument.type_().clone())
                    .collect(),
                result_type,
            ),
        )
    }
}
