use super::{
    instruction_builder::InstructionBuilder, name_generator::NameGenerator, typed_expression::*,
};
use crate::{
    ir::*,
    types::{self, Type},
};
use std::{cell::RefCell, rc::Rc};

#[derive(Clone, Debug, Default)]
pub struct ModuleBuilder {
    name_generator: Rc<RefCell<NameGenerator>>,
    variable_declarations: Rc<RefCell<Vec<VariableDeclaration>>>,
    function_declarations: Rc<RefCell<Vec<FunctionDeclaration>>>,
    variable_definitions: Rc<RefCell<Vec<VariableDefinition>>>,
    function_definitions: Rc<RefCell<Vec<FunctionDefinition>>>,
}

impl ModuleBuilder {
    pub fn new() -> Self {
        Self {
            name_generator: Rc::new(NameGenerator::new("_fmm_").into()),
            variable_declarations: RefCell::new(vec![]).into(),
            function_declarations: RefCell::new(vec![]).into(),
            variable_definitions: RefCell::new(vec![]).into(),
            function_definitions: RefCell::new(vec![]).into(),
        }
    }

    pub fn as_module(&self) -> Module {
        Module::new(
            self.variable_declarations.as_ref().borrow().clone(),
            self.function_declarations.as_ref().borrow().clone(),
            self.variable_definitions.as_ref().borrow().clone(),
            self.function_definitions.as_ref().borrow().clone(),
        )
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

        TypedExpression::new(Variable::new(name), types::Pointer::new(type_))
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

        TypedExpression::new(Variable::new(name), type_)
    }

    pub fn define_variable(
        &self,
        name: impl Into<String>,
        body: impl Into<TypedExpression>,
        options: VariableDefinitionOptions,
    ) -> TypedExpression {
        let name = name.into();
        let body = body.into();

        self.variable_definitions
            .borrow_mut()
            .push(VariableDefinition::new(
                &name,
                body.expression().clone(),
                body.type_().clone(),
                options,
            ));

        TypedExpression::new(
            Variable::new(name),
            types::Pointer::new(body.type_().clone()),
        )
    }

    pub fn define_anonymous_variable(
        &self,
        body: impl Into<TypedExpression>,
        options: VariableDefinitionOptions,
    ) -> TypedExpression {
        self.define_variable(
            self.generate_name(),
            body,
            options.set_linkage(Linkage::Internal),
        )
    }

    pub fn define_function<E>(
        &self,
        name: impl Into<String>,
        arguments: Vec<Argument>,
        result_type: impl Into<Type>,
        body: impl Fn(InstructionBuilder) -> Result<Block, E>,
        options: FunctionDefinitionOptions,
    ) -> Result<TypedExpression, E> {
        let name = name.into();
        let function_definition = FunctionDefinition::new(
            &name,
            arguments,
            result_type.into(),
            body(InstructionBuilder::new(self.name_generator.clone()))?,
            options,
        );
        let type_ = function_definition.type_().clone();

        self.function_definitions
            .borrow_mut()
            .push(function_definition);

        Ok(TypedExpression::new(Variable::new(name), type_))
    }

    pub fn define_anonymous_function<E>(
        &self,
        arguments: Vec<Argument>,
        result_type: impl Into<Type>,
        body: impl Fn(InstructionBuilder) -> Result<Block, E>,
        options: FunctionDefinitionOptions,
    ) -> Result<TypedExpression, E> {
        self.define_function(
            self.generate_name(),
            arguments,
            result_type,
            body,
            options.set_linkage(Linkage::Internal),
        )
    }

    pub fn generate_name(&self) -> String {
        self.name_generator.borrow_mut().generate()
    }
}
