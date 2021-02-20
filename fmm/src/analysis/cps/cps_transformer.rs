use crate::ir::*;
use crate::types::{self, CallingConvention, Type};

const STACK_POINTER_ARGUMENT_NAME: &str = "_s";
const CONTINUATION_ARGUMENT_NAME: &str = "_k";
const RESULT_TYPE: types::Primitive = types::Primitive::PointerInteger;
const RESULT_NAME: &str = "_result";

pub struct CpsTransformer {
    continuation_index: usize,
    function_definitions: Vec<FunctionDefinition>,
}

impl CpsTransformer {
    pub fn new() -> Self {
        Self {
            continuation_index: 0,
            function_definitions: vec![],
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
        if declaration.type_().calling_convention() == CallingConvention::Tail {
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
        if definition.calling_convention() == CallingConvention::Tail {
            FunctionDefinition::new(
                definition.name(),
                vec![
                    Argument::new(STACK_POINTER_ARGUMENT_NAME, self.get_stack_pointer_type()),
                    Argument::new(
                        CONTINUATION_ARGUMENT_NAME,
                        self.create_continuation_type(definition.result_type()),
                    ),
                ]
                .into_iter()
                .chain(definition.arguments().iter().cloned())
                .collect(),
                self.transform_block(definition.body()),
                RESULT_TYPE,
                CallingConvention::Direct,
                definition.is_global(),
            )
        } else {
            definition.clone()
        }
    }

    fn transform_block(&mut self, block: &Block) -> Block {
        let (instructions, terminal_instruction) =
            self.transform_instructions(block.instructions(), block.terminal_instruction());

        Block::new(instructions, terminal_instruction)
    }

    fn transform_instructions(
        &mut self,
        instructions: &[Instruction],
        terminal_instruction: &TerminalInstruction,
    ) -> (Vec<Instruction>, TerminalInstruction) {
        match instructions {
            [] => {
                let return_ = terminal_instruction.to_return().unwrap();

                (
                    vec![Call::new(
                        self.create_continuation_type(return_.type_()),
                        Variable::new(CONTINUATION_ARGUMENT_NAME),
                        vec![
                            Variable::new(STACK_POINTER_ARGUMENT_NAME).into(),
                            return_.expression().clone(),
                        ],
                        RESULT_NAME,
                    )
                    .into()],
                    Return::new(RESULT_TYPE, Variable::new(RESULT_NAME)).into(),
                )
            }
            [instruction, ..] => {
                if let Instruction::Call(call) = instruction {
                    if call.type_().calling_convention() == CallingConvention::Tail {
                        let continuation = self.create_continuation(
                            call,
                            &instructions[1..],
                            terminal_instruction,
                        );

                        return (
                            vec![Call::new(
                                self.transform_function_type(call.type_()),
                                call.function().clone(),
                                vec![
                                    Variable::new(STACK_POINTER_ARGUMENT_NAME).into(),
                                    continuation,
                                ]
                                .into_iter()
                                .chain(call.arguments().iter().cloned())
                                .collect(),
                                RESULT_NAME,
                            )
                            .into()],
                            Return::new(RESULT_TYPE, Variable::new(RESULT_NAME)).into(),
                        );
                    }
                }

                let (instructions, terminal_instruction) =
                    self.transform_instructions(&instructions[1..], terminal_instruction);

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

    fn create_continuation(
        &mut self,
        call: &Call,
        instructions: &[Instruction],
        terminal_instruction: &TerminalInstruction,
    ) -> Expression {
        let name = self.generate_continuation_name();
        let block = self.transform_block(&Block::new(
            instructions.to_vec(),
            terminal_instruction.clone(),
        ));

        self.function_definitions.push(FunctionDefinition::new(
            &name,
            vec![
                Argument::new(STACK_POINTER_ARGUMENT_NAME, self.get_stack_pointer_type()),
                Argument::new(call.name(), call.type_().result().clone()),
            ],
            block,
            RESULT_TYPE,
            CallingConvention::Direct,
            false,
        ));

        Variable::new(name).into()
    }

    fn transform_function_type(&self, type_: &types::Function) -> types::Function {
        types::Function::new(
            vec![
                self.get_stack_pointer_type(),
                self.create_continuation_type(type_.result()).into(),
            ]
            .into_iter()
            .chain(type_.arguments().iter().cloned())
            .collect(),
            RESULT_TYPE,
            CallingConvention::Direct,
        )
    }

    fn create_continuation_type(&self, result_type: &Type) -> types::Function {
        types::Function::new(
            vec![self.get_stack_pointer_type(), result_type.clone()],
            RESULT_TYPE,
            CallingConvention::Direct,
        )
    }

    fn generate_continuation_name(&mut self) -> String {
        let name = format!("_k{}", self.continuation_index);

        self.continuation_index += 1;

        name
    }

    fn get_stack_pointer_type(&self) -> Type {
        types::Pointer::new(types::Primitive::Integer8).into()
    }
}
