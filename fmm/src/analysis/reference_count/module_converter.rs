use super::global_variable_tag::tag_expression;
use super::{
    expression_converter::ExpressionConverter,
    expression_lifetime_manager::ExpressionLifetimeManager,
    record_rc_function_creator::RecordRcFunctionCreator,
};
use crate::{
    analysis::collect_types,
    build::{InstructionBuilder, NameGenerator, TypedExpression},
    ir::*,
    types::{self, Type},
};
use std::rc::Rc;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

pub struct ModuleConverter {
    expression_converter: Rc<ExpressionConverter>,
    expression_lifetime_manager: Rc<ExpressionLifetimeManager>,
    record_rc_function_creator: Rc<RecordRcFunctionCreator>,
    name_generator: Rc<RefCell<NameGenerator>>,
}

impl ModuleConverter {
    pub fn new(
        expression_converter: Rc<ExpressionConverter>,
        expression_lifetime_manager: Rc<ExpressionLifetimeManager>,
        record_rc_function_creator: Rc<RecordRcFunctionCreator>,
        name_generator: Rc<RefCell<NameGenerator>>,
    ) -> Self {
        Self {
            expression_converter,
            expression_lifetime_manager,
            record_rc_function_creator,
            name_generator,
        }
    }

    pub fn convert(&self, module: &Module) -> Module {
        let global_variables = self.collect_global_variables(module);

        Module::new(
            module.variable_declarations().to_vec(),
            module.function_declarations().to_vec(),
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
            .map(|(name, type_)| (name, types::Pointer::new(type_).into()))
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
        global_variables: &HashMap<String, Type>,
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
        global_variables: &HashMap<String, Type>,
    ) -> FunctionDefinition {
        FunctionDefinition::new(
            definition.name(),
            definition.arguments().to_vec(),
            Block::new(
                global_variables
                    .iter()
                    .filter(|(name, _)| {
                        !definition
                            .arguments()
                            .iter()
                            .any(|argument| argument.name() == name.as_str())
                    })
                    .map(|(name, type_)| {
                        PassThrough::new(
                            type_.clone(),
                            tag_expression(&Variable::new(name).into(), type_, global_variables),
                            name,
                        )
                        .into()
                    })
                    .chain(
                        self.convert_instructions(
                            definition.body().instructions(),
                            definition.body().terminal_instruction(),
                            &definition
                                .arguments()
                                .iter()
                                .map(|argument| argument.name().into())
                                .collect(),
                            &HashSet::new(),
                        )
                        .0,
                    )
                    .collect(),
                definition.body().terminal_instruction().clone(),
            ),
            definition.result_type().clone(),
            definition.calling_convention(),
            definition.is_global(),
        )
    }

    fn convert_instructions(
        &self,
        instructions: &[Instruction],
        terminal_instruction: &TerminalInstruction,
        owned_variables: &HashSet<String>,
        used_variables: &HashSet<String>,
    ) -> (Vec<Instruction>, HashSet<String>) {
        match instructions {
            [] => self.convert_terminal_instruction(terminal_instruction, used_variables),
            [instruction, ..] => {
                let rest_instructions = &instructions[1..];
                let owned_variables = owned_variables
                    .clone()
                    .into_iter()
                    .chain(self.get_owned_variable_from_instruction(instruction))
                    .collect();

                let (rest_instructions, used_variables) = self.convert_instructions(
                    rest_instructions,
                    terminal_instruction,
                    &owned_variables,
                    used_variables,
                );

                self.convert_instruction(instruction, &owned_variables, &used_variables)
                    .into_iter()
                    .chain(rest_instructions)
                    .collect()
            }
        }
    }

    fn get_owned_variable_from_instruction(&self, instruction: &Instruction) -> Option<String> {
        match instruction {
            Instruction::AllocateHeap(_)
            | Instruction::AllocateStack(_)
            | Instruction::AtomicLoad(_)
            | Instruction::Call(_)
            | Instruction::DeconstructRecord(_)
            | Instruction::DeconstructUnion(_)
            | Instruction::If(_)
            | Instruction::Load(_)
            | Instruction::PassThrough(_)
            | Instruction::ReallocateHeap(_) => instruction.name().map(|name| name.into()),
            Instruction::ArithmeticOperation(_)
            | Instruction::AtomicOperation(_)
            | Instruction::AtomicStore(_)
            // TODO Drop old values after compare-and-swap instructions.
            | Instruction::CompareAndSwap(_)
            | Instruction::ComparisonOperation(_)
            | Instruction::FreeHeap(_)
            | Instruction::PointerAddress(_)
            | Instruction::RecordAddress(_)
            | Instruction::Store(_)
            | Instruction::UnionAddress(_) => None,
        }
    }

    // Returned instructions include instructions of arguments.
    fn convert_instruction(
        &self,
        instruction: &Instruction,
        owned_variables: &HashSet<String>,
        used_variables: &HashSet<String>,
    ) -> Vec<Instruction> {
        let builder = InstructionBuilder::new(self.name_generator.clone());

        match instruction {
            Instruction::AllocateHeap(allocate) => {
                let record_type = types::Record::new(vec![
                    types::Primitive::PointerInteger.into(),
                    allocate.type_().clone(),
                ]);
                let pointer = builder.allocate_heap(record_type.clone());

                builder.store(Undefined::new(record_type.clone()), pointer.clone());

                builder
                    .into_instructions()
                    .into_iter()
                    .chain(vec![RecordAddress::new(
                        record_type.clone(),
                        pointer.expression().clone(),
                        1,
                        allocate.name(),
                    )
                    .into()])
                    .collect()
            }
            Instruction::Store(store) => {
                let value = builder.load(TypedExpression::new(
                    store.pointer().clone(),
                    types::Pointer::new(store.type_().clone()),
                ));

                builder
                    .into_instructions()
                    .into_iter()
                    .chain(
                        self.expression_lifetime_manager
                            .drop_expression(value.expression(), value.type_()),
                    )
                    .chain(
                        self.expression_lifetime_manager
                            .clone_expression(store.value(), store.type_()),
                    )
                    .chain(vec![store.clone().into()])
                    .collect()
            }
            Instruction::AllocateStack(_)
            | Instruction::ArithmeticOperation(_)
            | Instruction::AtomicLoad(_)
            | Instruction::AtomicOperation(_)
            | Instruction::AtomicStore(_)
            | Instruction::Call(_)
            | Instruction::CompareAndSwap(_)
            | Instruction::ComparisonOperation(_)
            | Instruction::DeconstructRecord(_)
            | Instruction::DeconstructUnion(_)
            | Instruction::If(_)
            | Instruction::Load(_)
            | Instruction::PassThrough(_)
            | Instruction::PointerAddress(_)
            | Instruction::ReallocateHeap(_)
            | Instruction::RecordAddress(_)
            | Instruction::UnionAddress(_) => todo!(),
            Instruction::FreeHeap(_) => vec![instruction.clone()],
        }
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
