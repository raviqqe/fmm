use super::{
    function_declaration::FunctionDeclaration, function_definition::FunctionDefinition,
    variable_declaration::VariableDeclaration, variable_definition::VariableDefinition,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    variable_declarations: Vec<VariableDeclaration>,
    function_declarations: Vec<FunctionDeclaration>,
    variable_definitions: Vec<VariableDefinition>,
    function_definitions: Vec<FunctionDefinition>,
}

impl Module {
    pub fn new(
        variable_declarations: Vec<VariableDeclaration>,
        function_declarations: Vec<FunctionDeclaration>,
        variable_definitions: Vec<VariableDefinition>,
        function_definitions: Vec<FunctionDefinition>,
    ) -> Self {
        Self {
            variable_declarations,
            function_declarations,
            variable_definitions,
            function_definitions,
        }
    }

    pub fn variable_declarations(&self) -> &[VariableDeclaration] {
        &self.variable_declarations
    }

    pub fn function_declarations(&self) -> &[FunctionDeclaration] {
        &self.function_declarations
    }

    pub fn variable_definitions(&self) -> &[VariableDefinition] {
        &self.variable_definitions
    }

    pub fn function_definitions(&self) -> &[FunctionDefinition] {
        &self.function_definitions
    }

    pub fn variable_declarations_mut(&mut self) -> &mut Vec<VariableDeclaration> {
        &mut self.variable_declarations
    }

    pub fn function_declarations_mut(&mut self) -> &mut Vec<FunctionDeclaration> {
        &mut self.function_declarations
    }

    pub fn variable_definitions_mut(&mut self) -> &mut Vec<VariableDefinition> {
        &mut self.variable_definitions
    }

    pub fn function_definitions_mut(&mut self) -> &mut Vec<FunctionDefinition> {
        &mut self.function_definitions
    }
}
