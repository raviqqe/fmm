use crate::ir::*;
use crate::types::{self, CallingConvention};

const CONTINUATION_ARGUMENT_NAME: &str = "_k";
const RESULT_TYPE: types::Primitive = types::Primitive::PointerInteger;
const RESULT_NAME: &str = "_result";

pub struct CpsTransformer {
    function_definitions: Vec<FunctionDefinition>,
}

impl CpsTransformer {
    pub fn new() -> Self {
        Self {
            function_definitions: vec![],
        }
    }

    pub fn transform(&mut self, module: &Module) -> Module {
        Module::new(
            module.variable_declarations().to_vec(),
            module.function_declarations().to_vec(),
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

    fn transform_function_definition(
        &mut self,
        definition: &FunctionDefinition,
    ) -> FunctionDefinition {
        if definition.calling_convention() == CallingConvention::Tail {
            FunctionDefinition::new(
                definition.name(),
                vec![Argument::new(
                    CONTINUATION_ARGUMENT_NAME,
                    types::Function::new(
                        vec![definition.result_type().clone()],
                        RESULT_TYPE,
                        CallingConvention::Direct,
                    ),
                )]
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
                        types::Function::new(
                            vec![return_.type_().clone()],
                            RESULT_TYPE,
                            CallingConvention::Direct,
                        ),
                        Variable::new(CONTINUATION_ARGUMENT_NAME),
                        vec![return_.expression().clone()],
                        CallingConvention::Direct,
                        RESULT_NAME,
                    )
                    .into()],
                    Return::new(RESULT_TYPE, Variable::new(RESULT_NAME)).into(),
                )
            }
            [instruction, ..] => {
                let (instructions, terminal_instruction) =
                    self.transform_instructions(&instructions[1..], terminal_instruction);

                if let Instruction::Call(call) = instruction {
                    if call.type_().calling_convention() == CallingConvention::Tail {
                        todo!()
                    }
                }

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
}
