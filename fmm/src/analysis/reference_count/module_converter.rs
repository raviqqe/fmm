use super::global_variable_tagger::tag_expression;
use super::{
    expression_lifetime_manager::ExpressionLifetimeManger,
    expression_reference_counter::ExpressionReferenceCounter,
    record_rc_function_creator::RecordRcFunctionCreator,
};
use crate::{analysis::collect_types, ir::*, types::Type};
use std::collections::HashSet;
use std::rc::Rc;

pub struct ModuleConverter {
    expression_reference_counter: Rc<ExpressionReferenceCounter>,
    expression_lifetime_manager: Rc<ExpressionLifetimeManger>,
    record_rc_function_creator: Rc<RecordRcFunctionCreator>,
}

impl ModuleConverter {
    pub fn new(
        expression_reference_counter: Rc<ExpressionReferenceCounter>,
        expression_lifetime_manager: Rc<ExpressionLifetimeManger>,
        record_rc_function_creator: Rc<RecordRcFunctionCreator>,
    ) -> Self {
        Self {
            expression_reference_counter,
            expression_lifetime_manager,
            record_rc_function_creator,
        }
    }

    pub fn convert(&self, module: &Module) -> Module {
        let global_variables = self.collect_global_variables(module);

        Module::new(
            module.variable_declarations().to_vec(),
            module.function_declarations().to_vec(),
            // TODO Tag static variables in variable definitions.
            module
                .variable_definitions()
                .iter()
                .map(|definition| self.convert_variable_definition(definition, &global_variables))
                .collect(),
            self.create_record_functions(module)
                .into_iter()
                .chain(module.function_definitions().iter().map(|definition| {
                    self.convert_function_definition(definition, &global_variables)
                }))
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

    fn create_record_functions(&self, module: &Module) -> Vec<FunctionDefinition> {
        collect_types(module)
            .into_iter()
            .flat_map(|type_| match type_ {
                Type::Record(record_type) => {
                    vec![
                        self.record_rc_function_creator
                            .create_record_clone_function(&record_type),
                        self.record_rc_function_creator
                            .create_record_drop_function(&record_type),
                    ]
                }
                _ => vec![],
            })
            .collect()
    }

    fn convert_variable_definition(
        &self,
        definition: &VariableDefinition,
        global_variables: &HashSet<String>,
    ) -> VariableDefinition {
        VariableDefinition::new(
            definition.name(),
            tag_expression(definition.body(), definition.type_(), global_variables),
            definition.type_().clone(),
            definition.is_mutable(),
            definition.is_global(),
        )
    }

    fn convert_function_definition(
        &self,
        definition: &FunctionDefinition,
        global_variables: &HashSet<String>,
    ) -> FunctionDefinition {
        // TODO Tag static variables.
        FunctionDefinition::new(
            definition.name(),
            definition.arguments().to_vec(),
            Block::new(
                self.convert_instructions(
                    definition.body().instructions(),
                    definition.body().terminal_instruction(),
                    &HashSet::new(),
                ),
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
        let (more_instructions, _used_variables) =
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
                let (instructions, used_variables) = self.expression_reference_counter.count(
                    branch.expression(),
                    branch.type_(),
                    used_variables,
                );

                (instructions, used_variables)
            }
            TerminalInstruction::Return(return_) => self.expression_reference_counter.count(
                return_.expression(),
                return_.type_(),
                used_variables,
            ),
            TerminalInstruction::Unreachable => (Default::default(), Default::default()),
        }
    }
}
