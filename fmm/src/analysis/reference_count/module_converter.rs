use super::expression_reference_counter::ExpressionReferenceCounter;
use crate::ir::*;
use std::collections::HashSet;
use std::rc::Rc;

pub struct ModuleConverter {
    expression_reference_counter: Rc<ExpressionReferenceCounter>,
}

impl ModuleConverter {
    pub fn new(expression_reference_counter: Rc<ExpressionReferenceCounter>) -> Self {
        Self {
            expression_reference_counter,
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

    fn collect_global_variables(&self, module: &Module) -> HashSet<String> {
        module
            .variable_declarations()
            .iter()
            .map(|declaration| declaration.name().into())
            .chain(
                module
                    .variable_definitions()
                    .iter()
                    .map(|definition| definition.name().into()),
            )
            .collect()
    }

    fn convert_function_definition(
        &self,
        definition: &FunctionDefinition,
        global_variables: &HashSet<String>,
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
                let (instructions, used_variables) = self.expression_reference_counter.convert(
                    branch.expression(),
                    branch.type_(),
                    used_variables,
                );

                (instructions, used_variables)
            }
            TerminalInstruction::Return(return_) => self.expression_reference_counter.convert(
                return_.expression(),
                return_.type_(),
                used_variables,
            ),
            TerminalInstruction::Unreachable => (Default::default(), Default::default()),
        }
    }
}
