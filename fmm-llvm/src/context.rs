use crate::{CompileError, InstructionConfiguration};
use fmm::types::Type;
use fnv::FnvHashMap;
use once_cell::sync::Lazy;

static DEFAULT_TARGET_TRIPLE: Lazy<String> = Lazy::new(|| {
    inkwell::targets::TargetMachine::get_default_triple()
        .as_str()
        .to_str()
        .unwrap()
        .into()
});

pub struct Context<'c> {
    inkwell: &'c inkwell::context::Context,
    target_machine: inkwell::targets::TargetMachine,
    instruction_configuration: InstructionConfiguration,
    types: FnvHashMap<Type, inkwell::types::BasicTypeEnum<'c>>,
}

impl<'c> Context<'c> {
    pub fn new(
        inkwell_context: &'c inkwell::context::Context,
        target_triple: Option<&str>,
        instruction_configuration: InstructionConfiguration,
    ) -> Result<Self, CompileError> {
        Ok(Self {
            inkwell: inkwell_context,
            target_machine: Self::create_target_machine(target_triple)?,
            instruction_configuration,
            types: Default::default(),
        })
    }

    pub fn inkwell(&self) -> &'c inkwell::context::Context {
        &self.inkwell
    }

    pub fn target_machine(&self) -> &inkwell::targets::TargetMachine {
        &self.target_machine
    }

    pub fn instruction_configuration(&self) -> &InstructionConfiguration {
        &self.instruction_configuration
    }

    fn create_target_machine(
        target_triple: Option<&str>,
    ) -> Result<inkwell::targets::TargetMachine, CompileError> {
        inkwell::targets::Target::initialize_all(&inkwell::targets::InitializationConfig::default());
        let target_triple =
            inkwell::targets::TargetTriple::create(target_triple.unwrap_or(&DEFAULT_TARGET_TRIPLE));

        inkwell::targets::Target::from_triple(&target_triple)?
            .create_target_machine(
                &target_triple,
                "",
                "",
                inkwell::OptimizationLevel::Aggressive,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .ok_or(CompileError::TargetMachineNotCreated)
    }

    pub fn types(&self) -> &FnvHashMap<Type, inkwell::types::BasicTypeEnum<'c>> {
        &self.types
    }

    pub fn types_mut(&mut self) -> &mut FnvHashMap<Type, inkwell::types::BasicTypeEnum<'c>> {
        &mut self.types
    }
}
