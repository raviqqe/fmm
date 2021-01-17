use super::block_state::BlockState;
use super::names::*;
use super::typed_expression::*;
use crate::ir::*;
use crate::types::{self, Type};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ModuleState {
    variable_declarations: Rc<RefCell<Vec<VariableDeclaration>>>,
    function_declarations: Rc<RefCell<Vec<FunctionDeclaration>>>,
    variable_definitions: Rc<RefCell<Vec<VariableDefinition>>>,
    function_definitions: Rc<RefCell<Vec<FunctionDefinition>>>,
}

impl ModuleState {
    pub fn new() -> Self {
        Self {
            variable_declarations: RefCell::new(vec![]).into(),
            function_declarations: RefCell::new(vec![]).into(),
            variable_definitions: RefCell::new(vec![]).into(),
            function_definitions: RefCell::new(vec![]).into(),
        }
    }

    pub fn declare_variable(
        &self,
        name: impl Into<String>,
        type_: impl Into<Type>,
    ) -> TypedExpression {
        let name = name.into();
        let type_ = type_.into();

        self.variable_declarations
            .borrow_mut()
            .push(VariableDeclaration::new(&name, type_.clone()));

        TypedExpression::new(Variable::new(name), types::Pointer::new(type_.clone()))
    }

    pub fn declare_function(
        &self,
        name: impl Into<String>,
        type_: types::Function,
    ) -> TypedExpression {
        let name = name.into();

        self.function_declarations
            .borrow_mut()
            .push(FunctionDeclaration::new(&name, type_.clone()));

        TypedExpression::new(Variable::new(name), type_.clone())
    }

    pub fn define_variable(
        &self,
        name: impl Into<String>,
        body: impl Into<TypedExpression>,
        mutable: bool,
    ) -> TypedExpression {
        let name = name.into();
        let body = body.into();

        self.variable_definitions
            .borrow_mut()
            .push(VariableDefinition::new(
                &name,
                body.expression().clone(),
                body.type_().clone(),
                mutable,
            ));

        TypedExpression::new(
            Variable::new(name),
            types::Pointer::new(body.type_().clone()),
        )
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
                body(BlockState::new(self.clone())),
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
