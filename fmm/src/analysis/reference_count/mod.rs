mod expressions;
mod reference_count;
mod utilities;

use crate::ir::*;
use crate::types::Type;
use std::collections::{HashMap, HashSet};

pub fn count_references(module: &Module) -> Module {
    convert_module(module)
}

pub fn convert_module(module: &Module) -> Module {
    let variables = collect_global_variables(module);

    Module::new(
        module.variable_declarations().to_vec(),
        module.function_declarations().to_vec(),
        module.variable_definitions().to_vec(),
        module
            .function_definitions()
            .iter()
            .map(|definition| convert_function_definition(definition, &variables))
            .collect(),
    )
}

fn collect_global_variables(module: &Module) -> HashMap<String, Type> {
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
    definition: &FunctionDefinition,
    variables: &HashMap<String, Type>,
) -> FunctionDefinition {
    FunctionDefinition::new(
        definition.name(),
        definition
            .arguments()
            .iter()
            .map(|argument| Argument::new(argument.name(), convert_argument_type(argument.type_())))
            .collect(),
        convert_block(definition.body(), variables),
        definition.result_type().clone(),
        definition.calling_convention(),
        definition.is_global(),
    )
}

fn convert_argument_type(type_: &Type) -> Type {
    // Pointers do not have to be converted because their representation is the same.
    match type_ {
        Type::Union(union) => add_type_header(type_).into(),
        _ => type_.clone(),
    }
}

fn convert_block(block: &Block, variables: &HashMap<String, Type>) -> Block {
    let (instructions, terminal_instruction) =
        convert_instructions(block.instructions(), block.terminal_instruction());

    Block::new(instructions, terminal_instruction)
}

fn convert_instructions(
    instructions: &[Instruction],
    terminal_instruction: &TerminalInstruction,
    variables: &HashMap<String, Type>,
) -> (Vec<Instruction>, TerminalInstruction) {
    let (more_instructions, terminal_instruction, used_variables) =
        convert_terminal_instruction(terminal_instruction, variables);

    // TODO Convert instructions.

    (instructions.to_vec(), terminal_instruction)
}

fn convert_terminal_instruction(
    instruction: &TerminalInstruction,
    variables: &HashMap<String, Type>,
) -> (Vec<Instruction>, TerminalInstruction, HashSet<String>) {
    match instruction {
        TerminalInstruction::Branch(branch) => {
            let (instructions, expression, used_variables) =
                convert_expression(branch.expression(), variables);

            (
                instructions,
                Branch::new(branch.type_().clone(), expression).into(),
                used_variables,
            )
        }
        TerminalInstruction::Return(return_) => {
            let (instructions, expression, used_variables) =
                convert_expression(return_.expression(), variables);

            (
                instructions,
                Return::new(return_.type_().clone(), expression).into(),
                used_variables,
            )
        }
        TerminalInstruction::Unreachable => (
            Default::default(),
            TerminalInstruction::Unreachable,
            Default::default(),
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::check_types;
    use crate::types::{self, CallingConvention};

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
                    record_type,
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
