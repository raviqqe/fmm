use super::expression_converter::ExpressionConverter;
use crate::ir::*;
use crate::types::Type;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub struct ModuleConverter {
    expression_converter: Rc<ExpressionConverter>,
}

impl ModuleConverter {
    pub fn new(expression_converter: Rc<ExpressionConverter>) -> Self {
        Self {
            expression_converter,
        }
    }

    pub fn convert(&self, module: &Module) -> Module {
        let global_variables = self.collect_global_variables(module);

        Module::new(
            module.variable_declarations().to_vec(),
            module.function_declarations().to_vec(),
            // TODO Tag static variables in variable definitions.
            module.variable_definitions().to_vec(),
            module
                .function_definitions()
                .iter()
                .map(|definition| self.convert_function_definition(definition, &global_variables))
                .collect(),
        )
    }

    fn collect_global_variables(&self, module: &Module) -> HashMap<String, Type> {
        module
            .variable_declarations()
            .iter()
            .map(|declaration| (declaration.name().into(), declaration.type_().clone()))
            .chain(
                module
                    .variable_definitions()
                    .iter()
                    .map(|definition| (definition.name().into(), definition.type_().clone())),
            )
            .collect()
    }

    fn convert_function_definition(
        &self,
        definition: &FunctionDefinition,
        global_variables: &HashMap<String, Type>,
    ) -> FunctionDefinition {
        // TODO Tag static variables.
        let instructions = self.convert_instructions(
            definition.body().instructions(),
            definition.body().terminal_instruction(),
            &HashSet::new(),
        );

        FunctionDefinition::new(
            definition.name(),
            definition.arguments().to_vec(),
            Block::new(
                instructions,
                definition.body().terminal_instruction().clone(),
            ),
            definition.result_type().clone(),
            definition.calling_convention(),
            definition.is_global(),
        )
    }

    fn convert_block(&self, block: &Block, used_variables: &HashSet<String>) -> Block {
        let instructions = self.convert_instructions(
            block.instructions(),
            block.terminal_instruction(),
            used_variables,
        );

        Block::new(instructions, block.terminal_instruction().clone())
    }

    fn convert_instructions(
        &self,
        instructions: &[Instruction],
        terminal_instruction: &TerminalInstruction,
        used_variables: &HashSet<String>,
    ) -> Vec<Instruction> {
        let (more_instructions, used_variables) =
            self.convert_terminal_instruction(terminal_instruction, used_variables);

        // TODO Convert instructions.

        instructions
            .iter()
            .cloned()
            .chain(more_instructions)
            .collect()
    }

    fn convert_terminal_instruction(
        &self,
        instruction: &TerminalInstruction,
        used_variables: &HashSet<String>,
    ) -> (Vec<Instruction>, HashSet<String>) {
        match instruction {
            TerminalInstruction::Branch(branch) => {
                let (instructions, used_variables) = self.expression_converter.convert(
                    branch.expression(),
                    branch.type_(),
                    used_variables,
                );

                (instructions, used_variables)
            }
            TerminalInstruction::Return(return_) => self.expression_converter.convert(
                return_.expression(),
                return_.type_(),
                used_variables,
            ),
            TerminalInstruction::Unreachable => (Default::default(), Default::default()),
        }
    }
}
