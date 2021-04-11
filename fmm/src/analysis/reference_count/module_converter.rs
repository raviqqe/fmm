use super::expression_mover::ExpressionMover;
use super::{
    error::ReferenceCountError, expression_dropper::ExpressionDropper,
    record_drop_function_creator::RecordDropFunctionCreator,
};
use super::{
    global_variable_tag::tag_expression, record_clone_function_creator::RecordCloneFunctionCreator,
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
    expression_mover: Rc<ExpressionMover>,
    expression_dropper: Rc<ExpressionDropper>,
    record_clone_function_creator: Rc<RecordCloneFunctionCreator>,
    record_drop_function_creator: Rc<RecordDropFunctionCreator>,
    name_generator: Rc<RefCell<NameGenerator>>,
}

impl ModuleConverter {
    pub fn new(
        expression_mover: Rc<ExpressionMover>,
        expression_dropper: Rc<ExpressionDropper>,
        record_clone_function_creator: Rc<RecordCloneFunctionCreator>,
        record_drop_function_creator: Rc<RecordDropFunctionCreator>,
        name_generator: Rc<RefCell<NameGenerator>>,
    ) -> Self {
        Self {
            expression_mover,
            expression_dropper,
            record_clone_function_creator,
            record_drop_function_creator,
            name_generator,
        }
    }

    pub fn convert(&self, module: &Module) -> Result<Module, ReferenceCountError> {
        let global_variables = self.collect_global_variables(module);

        Ok(Module::new(
            module.variable_declarations().to_vec(),
            module.function_declarations().to_vec(),
            module
                .variable_definitions()
                .iter()
                .map(|definition| self.convert_variable_definition(definition, &global_variables))
                .collect::<Result<_, _>>()?,
            self.create_record_functions(module)
                .into_iter()
                .chain(
                    module
                        .function_definitions()
                        .iter()
                        .map(|definition| {
                            self.convert_function_definition(definition, &global_variables)
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                )
                .collect(),
        ))
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
                        self.record_clone_function_creator.create(&record_type),
                        self.record_drop_function_creator.create(&record_type),
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
    ) -> Result<VariableDefinition, ReferenceCountError> {
        Ok(VariableDefinition::new(
            definition.name(),
            tag_expression(definition.body(), definition.type_(), global_variables)?,
            definition.type_().clone(),
            definition.is_mutable(),
            definition.is_global(),
        ))
    }

    fn convert_function_definition(
        &self,
        definition: &FunctionDefinition,
        global_variables: &HashMap<String, Type>,
    ) -> Result<FunctionDefinition, ReferenceCountError> {
        let (instructions, _) = self.convert_instructions(
            definition.body().instructions(),
            definition.body().terminal_instruction(),
            &definition
                .arguments()
                .iter()
                .map(|argument| argument.name().into())
                .collect(),
            &HashSet::new(),
        )?;

        Ok(FunctionDefinition::new(
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
                        Ok(PassThrough::new(
                            type_.clone(),
                            tag_expression(&Variable::new(name).into(), type_, global_variables)?,
                            name,
                        )
                        .into())
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .chain(instructions)
                    .collect(),
                definition.body().terminal_instruction().clone(),
            ),
            definition.result_type().clone(),
            definition.calling_convention(),
            definition.is_global(),
        ))
    }

    fn convert_instructions(
        &self,
        instructions: &[Instruction],
        terminal_instruction: &TerminalInstruction,
        owned_variables: &HashSet<String>,
        moved_variables: &HashSet<String>,
    ) -> Result<(Vec<Instruction>, HashSet<String>), ReferenceCountError> {
        Ok(match instructions {
            [] => self.move_terminal_instruction_arguments(
                terminal_instruction,
                owned_variables,
                moved_variables,
            )?,
            [instruction, ..] => {
                let (rest_instructions, moved_variables) = self.convert_instructions(
                    &instructions[1..],
                    terminal_instruction,
                    &owned_variables
                        .clone()
                        .into_iter()
                        .chain(self.get_owned_variable_from_instruction(instruction))
                        .collect(),
                    moved_variables,
                )?;
                let (instructions, moved_variables) = self.move_instruction_arguments(
                    instruction,
                    &owned_variables,
                    &moved_variables,
                )?;

                (
                    instructions.into_iter().chain(rest_instructions).collect(),
                    moved_variables,
                )
            }
        })
    }

    fn get_owned_variable_from_instruction(&self, instruction: &Instruction) -> Option<String> {
        match instruction {
            Instruction::AllocateHeap(_)
            | Instruction::AtomicLoad(_)
            | Instruction::Call(_)
            | Instruction::DeconstructRecord(_)
            | Instruction::DeconstructUnion(_)
            | Instruction::If(_)
            | Instruction::Load(_)
            | Instruction::PassThrough(_)
            | Instruction::ReallocateHeap(_) => instruction.name().map(|name| name.into()),
            Instruction::AllocateStack(_)
            | Instruction::ArithmeticOperation(_)
            | Instruction::AtomicOperation(_)
            | Instruction::AtomicStore(_)
            | Instruction::CompareAndSwap(_)
            | Instruction::ComparisonOperation(_)
            | Instruction::FreeHeap(_)
            | Instruction::PointerAddress(_)
            | Instruction::RecordAddress(_)
            | Instruction::Store(_)
            | Instruction::UnionAddress(_) => None,
        }
    }

    // Returned instructions include original instructions.
    fn move_instruction_arguments(
        &self,
        instruction: &Instruction,
        owned_variables: &HashSet<String>,
        moved_variables: &HashSet<String>,
    ) -> Result<(Vec<Instruction>, HashSet<String>), ReferenceCountError> {
        let builder = InstructionBuilder::new(self.name_generator.clone());

        Ok(match instruction {
            Instruction::AllocateHeap(allocate) => {
                if moved_variables.contains(allocate.name()) {
                    let record_type = types::Record::new(vec![
                        types::Primitive::PointerInteger.into(),
                        allocate.type_().clone(),
                    ]);
                    let pointer = builder.allocate_heap(record_type.clone());

                    builder.store(Undefined::new(record_type.clone()), pointer.clone());

                    (
                        builder
                            .into_instructions()
                            .into_iter()
                            .chain(vec![RecordAddress::new(
                                record_type,
                                pointer.expression().clone(),
                                1,
                                allocate.name(),
                            )
                            .into()])
                            .collect(),
                        moved_variables.clone(),
                    )
                } else {
                    (vec![], moved_variables.clone())
                }
            }
            Instruction::AllocateStack(allocate) => (
                vec![
                    allocate.clone().into(),
                    Store::new(
                        allocate.type_().clone(),
                        Undefined::new(allocate.type_().clone()),
                        Variable::new(allocate.name()),
                    )
                    .into(),
                ],
                moved_variables.clone(),
            ),
            Instruction::AtomicLoad(load) => {
                let moved_variables = self.expression_mover.move_expression(
                    &builder,
                    &Variable::new(load.name()).into(),
                    load.type_(),
                    owned_variables,
                    moved_variables,
                )?;

                (
                    vec![load.clone().into()]
                        .into_iter()
                        .chain(builder.into_instructions())
                        .collect(),
                    moved_variables,
                )
            }
            Instruction::AtomicStore(store) => {
                self.expression_dropper.drop_expression(
                    &builder,
                    &builder.load(TypedExpression::new(
                        store.pointer().clone(),
                        types::Pointer::new(store.type_().clone()),
                    )),
                );
                let moved_variables = self.expression_mover.move_expression(
                    &builder,
                    store.value(),
                    store.type_(),
                    owned_variables,
                    moved_variables,
                )?;

                (
                    builder
                        .into_instructions()
                        .into_iter()
                        .chain(vec![store.clone().into()])
                        .collect(),
                    moved_variables.clone(),
                )
            }
            Instruction::Call(call) => {
                let mut moved_variables = moved_variables.clone();

                for (expression, type_) in
                    call.arguments().iter().zip(call.type_().arguments()).rev()
                {
                    moved_variables = self.expression_mover.move_expression(
                        &builder,
                        expression,
                        type_,
                        owned_variables,
                        &moved_variables,
                    )?;
                }

                (
                    builder
                        .into_instructions()
                        .into_iter()
                        .chain(vec![call.clone().into()])
                        .collect(),
                    moved_variables,
                )
            }
            Instruction::DeconstructRecord(deconstruct) => {
                let moved_variables = self.expression_mover.move_expression(
                    &builder,
                    &Variable::new(deconstruct.name()).into(),
                    &deconstruct.type_().elements()[deconstruct.element_index()],
                    owned_variables,
                    moved_variables,
                )?;

                (
                    vec![deconstruct.clone().into()]
                        .into_iter()
                        .chain(builder.into_instructions())
                        .collect(),
                    moved_variables,
                )
            }
            Instruction::DeconstructUnion(_) => Err(ReferenceCountError::UnionNotSupported)?,
            Instruction::If(if_) => {
                let convert_block = |block: &Block| {
                    let (instructions, moved_variables) = self.convert_instructions(
                        block.instructions(),
                        block.terminal_instruction(),
                        owned_variables,
                        moved_variables,
                    )?;

                    Ok((instructions, moved_variables))
                };
                let (then_instructions, then_moved_variables) = convert_block(if_.then())?;
                let (else_instructions, else_moved_variables) = convert_block(if_.else_())?;

                let create_block = |block: &Block,
                                    instructions,
                                    self_moved_variables: &HashSet<String>,
                                    other_moved_variables: &HashSet<String>|
                 -> Result<_, ReferenceCountError> {
                    let builder = InstructionBuilder::new(self.name_generator.clone());

                    for variable in other_moved_variables.difference(self_moved_variables) {
                        self.expression_dropper.drop_expression(variable)?;
                    }

                    Ok(Block::new(
                        builder
                            .into_instructions()
                            .into_iter()
                            .chain(instructions)
                            .collect(),
                        block.terminal_instruction().clone(),
                    ))
                };

                (
                    vec![If::new(
                        if_.type_().clone(),
                        if_.condition().clone(),
                        create_block(
                            if_.then(),
                            then_instructions,
                            &then_moved_variables,
                            &else_moved_variables,
                        )?,
                        create_block(
                            if_.else_(),
                            else_instructions,
                            &else_moved_variables,
                            &then_moved_variables,
                        )?,
                        if_.name(),
                    )
                    .into()],
                    then_moved_variables
                        .into_iter()
                        .chain(else_moved_variables)
                        .collect(),
                )
            }
            Instruction::Load(load) => {
                let moved_variables = self.expression_mover.move_expression(
                    &builder,
                    &Variable::new(load.name()).into(),
                    load.type_(),
                    owned_variables,
                    moved_variables,
                )?;

                (
                    vec![load.clone().into()]
                        .into_iter()
                        .chain(builder.into_instructions())
                        .collect(),
                    moved_variables,
                )
            }
            Instruction::PassThrough(pass) => {
                let moved_variables = self.expression_mover.move_expression(
                    &builder,
                    pass.expression(),
                    pass.type_(),
                    owned_variables,
                    moved_variables,
                )?;

                (
                    builder
                        .into_instructions()
                        .into_iter()
                        .chain(vec![pass.clone().into()])
                        .collect(),
                    moved_variables,
                )
            }
            Instruction::Store(store) => {
                self.expression_dropper.drop_expression(
                    &builder,
                    &builder.load(TypedExpression::new(
                        store.pointer().clone(),
                        types::Pointer::new(store.type_().clone()),
                    )),
                );
                let moved_variables = self.expression_mover.move_expression(
                    &builder,
                    store.value(),
                    store.type_(),
                    owned_variables,
                    moved_variables,
                )?;

                (
                    builder
                        .into_instructions()
                        .into_iter()
                        .chain(vec![store.clone().into()])
                        .collect(),
                    moved_variables.clone(),
                )
            }
            Instruction::ArithmeticOperation(_)
            | Instruction::AtomicOperation(_)
            | Instruction::CompareAndSwap(_)
            | Instruction::ComparisonOperation(_)
            | Instruction::FreeHeap(_)
            | Instruction::PointerAddress(_)
            | Instruction::ReallocateHeap(_)
            | Instruction::RecordAddress(_)
            | Instruction::UnionAddress(_) => (vec![instruction.clone()], moved_variables.clone()),
        })
    }

    fn move_terminal_instruction_arguments(
        &self,
        instruction: &TerminalInstruction,
        owned_variables: &HashSet<String>,
        moved_variables: &HashSet<String>,
    ) -> Result<(Vec<Instruction>, HashSet<String>), ReferenceCountError> {
        let builder = InstructionBuilder::new(self.name_generator.clone());

        let moved_variables = match instruction {
            TerminalInstruction::Branch(branch) => self.expression_mover.move_expression(
                &builder,
                branch.expression(),
                branch.type_(),
                owned_variables,
                moved_variables,
            )?,
            TerminalInstruction::Return(return_) => self.expression_mover.move_expression(
                &builder,
                return_.expression(),
                return_.type_(),
                owned_variables,
                moved_variables,
            )?,
            TerminalInstruction::Unreachable => Default::default(),
        };

        Ok((builder.into_instructions(), moved_variables))
    }
}
