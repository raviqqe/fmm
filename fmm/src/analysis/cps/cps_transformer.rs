use super::free_variables::collect_free_variables;
use super::stack::{pop_from_stack, push_to_stack, STACK_TYPE};
use crate::build::{self, InstructionBuilder, NameGenerator};
use crate::ir::*;
use crate::types::{self, CallingConvention, Type};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

const STACK_ARGUMENT_NAME: &str = "_s";
const CONTINUATION_ARGUMENT_NAME: &str = "_k";
const RESULT_NAME: &str = "_result";

pub struct CpsTransformer {
    name_generator: Rc<RefCell<NameGenerator>>,
    continuation_index: usize,
    function_definitions: Vec<FunctionDefinition>,
    result_type: Type,
}

impl CpsTransformer {
    pub fn new(result_type: impl Into<Type>) -> Self {
        Self {
            name_generator: Rc::new(NameGenerator::new("_cps_").into()),
            continuation_index: 0,
            function_definitions: vec![],
            result_type: result_type.into(),
        }
    }

    pub fn transform(&mut self, module: &Module) -> Module {
        Module::new(
            module.variable_declarations().to_vec(),
            module
                .function_declarations()
                .iter()
                .map(|declaration| self.transform_function_declaration(declaration))
                .collect(),
            module.variable_definitions().to_vec(),
            module
                .function_definitions()
                .iter()
                .map(|definition| self.transform_function_definition(definition))
                .collect::<Vec<_>>()
                .into_iter()
                .chain(self.function_definitions.drain(..))
                .collect(),
        )
    }

    fn transform_function_declaration(
        &self,
        declaration: &FunctionDeclaration,
    ) -> FunctionDeclaration {
        if declaration.type_().calling_convention() == CallingConvention::Source {
            FunctionDeclaration::new(
                declaration.name(),
                self.transform_function_type(declaration.type_()),
            )
        } else {
            declaration.clone()
        }
    }

    fn transform_function_definition(
        &mut self,
        definition: &FunctionDefinition,
    ) -> FunctionDefinition {
        if definition.calling_convention() == CallingConvention::Source {
            let continuation_type = self.create_continuation_type(definition.result_type());

            FunctionDefinition::new(
                definition.name(),
                vec![
                    Argument::new(STACK_ARGUMENT_NAME, STACK_TYPE.clone()),
                    Argument::new(CONTINUATION_ARGUMENT_NAME, continuation_type.clone()),
                ]
                .into_iter()
                .chain(definition.arguments().iter().cloned())
                .collect(),
                self.transform_block(
                    definition.body(),
                    &definition
                        .arguments()
                        .iter()
                        .map(|argument| (argument.name().into(), argument.type_().clone()))
                        .chain(vec![(
                            CONTINUATION_ARGUMENT_NAME.into(),
                            continuation_type.into(),
                        )])
                        .collect(),
                ),
                self.result_type.clone(),
                CallingConvention::Target,
                definition.is_global(),
            )
        } else {
            definition.clone()
        }
    }

    fn transform_block(&mut self, block: &Block, local_variables: &HashMap<String, Type>) -> Block {
        let (instructions, terminal_instruction) = self.transform_instructions(
            block.instructions(),
            block.terminal_instruction(),
            local_variables,
        );

        Block::new(instructions, terminal_instruction)
    }

    fn transform_instructions(
        &mut self,
        instructions: &[Instruction],
        terminal_instruction: &TerminalInstruction,
        local_variables: &HashMap<String, Type>,
    ) -> (Vec<Instruction>, TerminalInstruction) {
        match instructions {
            [] => {
                let return_ = terminal_instruction.to_return().unwrap();

                (
                    vec![Call::new(
                        self.create_continuation_type(return_.type_()),
                        Variable::new(CONTINUATION_ARGUMENT_NAME),
                        vec![
                            Variable::new(STACK_ARGUMENT_NAME).into(),
                            return_.expression().clone(),
                        ],
                        RESULT_NAME,
                    )
                    .into()],
                    Return::new(self.result_type.clone(), Variable::new(RESULT_NAME)).into(),
                )
            }
            [instruction, ..] => {
                let instructions = &instructions[1..];

                if let Instruction::Call(call) = instruction {
                    if call.type_().calling_convention() == CallingConvention::Source {
                        let environment = self.get_continuation_environment(
                            instructions,
                            terminal_instruction,
                            local_variables,
                        );
                        let continuation = self.create_continuation(
                            call,
                            instructions,
                            terminal_instruction,
                            &environment,
                        );

                        let builder = InstructionBuilder::new(self.name_generator.clone());

                        push_to_stack(
                            &builder,
                            build::variable(STACK_ARGUMENT_NAME, STACK_TYPE.clone()),
                            self.get_environment_record(&environment),
                        );

                        return (
                            builder
                                .into_instructions()
                                .into_iter()
                                .chain(vec![Call::new(
                                    self.transform_function_type(call.type_()),
                                    call.function().clone(),
                                    vec![Variable::new(STACK_ARGUMENT_NAME).into(), continuation]
                                        .into_iter()
                                        .chain(call.arguments().iter().cloned())
                                        .collect(),
                                    RESULT_NAME,
                                )
                                .into()])
                                .collect(),
                            Return::new(self.result_type.clone(), Variable::new(RESULT_NAME))
                                .into(),
                        );
                    }
                }

                let (instructions, terminal_instruction) = self.transform_instructions(
                    instructions,
                    terminal_instruction,
                    &local_variables
                        .clone()
                        .into_iter()
                        .chain(instruction.name().and_then(|name| {
                            instruction.result_type().map(|type_| (name.into(), type_))
                        }))
                        .collect(),
                );

                (
                    vec![instruction.clone()]
                        .into_iter()
                        .chain(instructions)
                        .collect(),
                    terminal_instruction,
                )
            }
        }
    }

    fn get_environment_record(&self, environment: &[(String, Type)]) -> Record {
        build::record(
            environment
                .iter()
                .map(|(name, type_)| build::variable(name.clone(), type_.clone()))
                .collect(),
        )
    }

    fn create_continuation(
        &mut self,
        call: &Call,
        instructions: &[Instruction],
        terminal_instruction: &TerminalInstruction,
        environment: &[(String, Type)],
    ) -> Expression {
        let name = self.generate_continuation_name();
        let block = self.transform_block(
            &Block::new(instructions.to_vec(), terminal_instruction.clone()),
            &vec![(call.name().into(), call.type_().result().clone())]
                .into_iter()
                .collect(),
        );

        self.function_definitions.push(FunctionDefinition::new(
            &name,
            vec![
                Argument::new(STACK_ARGUMENT_NAME, STACK_TYPE.clone()),
                Argument::new(call.name(), call.type_().result().clone()),
            ],
            Block::new(
                {
                    let builder = InstructionBuilder::new(self.name_generator.clone());

                    let environment_record_type =
                        self.get_environment_record(&environment).type_().clone();
                    let environment_record = pop_from_stack(
                        &builder,
                        build::variable(STACK_ARGUMENT_NAME, STACK_TYPE.clone()),
                        &environment_record_type.clone().into(),
                    );

                    builder
                        .into_instructions()
                        .into_iter()
                        .chain(environment.iter().enumerate().map(|(index, (name, _))| {
                            DeconstructRecord::new(
                                environment_record_type.clone(),
                                environment_record.expression().clone(),
                                index,
                                name,
                            )
                            .into()
                        }))
                        .chain(block.instructions().iter().cloned())
                        .collect()
                },
                block.terminal_instruction().clone(),
            ),
            self.result_type.clone(),
            CallingConvention::Target,
            false,
        ));

        Variable::new(name).into()
    }

    fn get_continuation_environment(
        &self,
        instructions: &[Instruction],
        terminal_instruction: &TerminalInstruction,
        local_variables: &HashMap<String, Type>,
    ) -> Vec<(String, Type)> {
        vec![(
            CONTINUATION_ARGUMENT_NAME.into(),
            local_variables[CONTINUATION_ARGUMENT_NAME].clone(),
        )]
        .into_iter()
        .chain(
            collect_free_variables(instructions, terminal_instruction)
                .iter()
                .flat_map(|name| {
                    local_variables
                        .get(name)
                        .map(|type_| (name.clone(), type_.clone()))
                }),
        )
        .collect()
    }

    fn transform_function_type(&self, type_: &types::Function) -> types::Function {
        types::Function::new(
            vec![
                STACK_TYPE.clone(),
                self.create_continuation_type(type_.result()).into(),
            ]
            .into_iter()
            .chain(type_.arguments().iter().cloned())
            .collect(),
            self.result_type.clone(),
            CallingConvention::Target,
        )
    }

    fn create_continuation_type(&self, result_type: &Type) -> types::Function {
        types::Function::new(
            vec![STACK_TYPE.clone(), result_type.clone()],
            self.result_type.clone(),
            CallingConvention::Target,
        )
    }

    fn generate_continuation_name(&mut self) -> String {
        let name = format!("_k{}", self.continuation_index);

        self.continuation_index += 1;

        name
    }
}
