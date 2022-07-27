mod calling_convention;
mod error;
mod expression;
mod instruction;
mod instruction_configuration;
mod type_;
mod union;

pub use error::CompileError;
use fmm::ir::*;
pub use instruction_configuration::InstructionConfiguration;
use instruction_configuration::InstructionFunctionSet;
use once_cell::sync::Lazy;

static DEFAULT_TARGET_TRIPLE: Lazy<String> = Lazy::new(|| {
    inkwell::targets::TargetMachine::get_default_triple()
        .as_str()
        .to_str()
        .unwrap()
        .into()
});

pub fn compile_to_bit_code(
    module: &Module,
    instruction_configuration: &InstructionConfiguration,
    target_triple: Option<&str>,
) -> Result<Vec<u8>, CompileError> {
    let target_machine = create_target_machine(target_triple)?;
    let context = inkwell::context::Context::create();

    let module = compile_module(&context, &target_machine, module, instruction_configuration)?;

    Ok(module.write_bitcode_to_memory().as_slice().to_vec())
}

pub fn compile_to_object(
    module: &Module,
    instruction_configuration: &InstructionConfiguration,
    target_triple: Option<&str>,
) -> Result<Vec<u8>, CompileError> {
    let target_machine = create_target_machine(target_triple)?;
    let context = inkwell::context::Context::create();

    let module = compile_module(&context, &target_machine, module, instruction_configuration)?;

    // TODO How can I set something equivalent to llvm::GuaranteedTailCallOpt in
    // C++? https://llvm.org/docs/LangRef.html#call-instruction
    Ok(target_machine
        .write_to_memory_buffer(&module, inkwell::targets::FileType::Object)?
        .as_slice()
        .to_vec())
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

fn compile_module<'c>(
    context: &'c inkwell::context::Context,
    target_machine: &inkwell::targets::TargetMachine,
    module: &Module,
    instruction_configuration: &InstructionConfiguration,
) -> Result<inkwell::module::Module<'c>, CompileError> {
    fmm::analysis::check(module)?;

    let target_data = target_machine.get_target_data();

    let llvm_module = context.create_module("");
    llvm_module.set_triple(&target_machine.get_triple());

    let mut variables = hamt::Map::new();

    let instruction_function_set = compile_heap_functions(
        &llvm_module,
        instruction_configuration,
        context,
        &target_data,
    );

    for declaration in module.variable_declarations() {
        let global = compile_variable_declaration(&llvm_module, declaration, context, &target_data);

        variables = variables.insert(declaration.name().into(), global.as_pointer_value().into());
    }

    for declaration in module.function_declarations() {
        let function =
            compile_function_declaration(&llvm_module, declaration, context, &target_data);

        variables = variables.insert(
            declaration.name().into(),
            function.as_global_value().as_pointer_value().into(),
        );
    }

    for definition in module.variable_definitions() {
        let global = declare_variable_definition(&llvm_module, definition, context, &target_data);

        variables = variables.insert(definition.name().into(), global.as_pointer_value().into());
    }

    for definition in module.function_definitions() {
        let function = declare_function_definition(&llvm_module, definition, context, &target_data);

        variables = variables.insert(
            definition.name().into(),
            function.as_global_value().as_pointer_value().into(),
        );
    }

    for definition in module.variable_definitions() {
        compile_variable_definition(&llvm_module, definition, &variables, context, &target_data);
    }

    for definition in module.function_definitions() {
        compile_function_definition(
            &llvm_module,
            definition,
            &variables,
            context,
            &target_data,
            &instruction_function_set,
        )?;
    }

    llvm_module.verify()?;

    Ok(llvm_module)
}

fn compile_heap_functions<'c>(
    module: &inkwell::module::Module<'c>,
    instruction_configuration: &InstructionConfiguration,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> InstructionFunctionSet<'c> {
    let pointer_type = context.i8_type().ptr_type(type_::DEFAULT_ADDRESS_SPACE);
    let pointer_integer_type = type_::compile_pointer_integer(context, target_data);

    InstructionFunctionSet {
        allocate_function: module.add_function(
            &instruction_configuration.allocate_function_name,
            pointer_type.fn_type(&[pointer_integer_type.into()], false),
            None,
        ),
        reallocate_function: module.add_function(
            &instruction_configuration.reallocate_function_name,
            pointer_type.fn_type(&[pointer_type.into(), pointer_integer_type.into()], false),
            None,
        ),
        free_function: module.add_function(
            &instruction_configuration.free_function_name,
            context.void_type().fn_type(&[pointer_type.into()], false),
            None,
        ),
        unreachable_function: instruction_configuration
            .unreachable_function_name
            .as_ref()
            .map(|name| module.add_function(name, context.void_type().fn_type(&[], false), None)),
    }
}

fn compile_variable_declaration<'c>(
    module: &inkwell::module::Module<'c>,
    declaration: &VariableDeclaration,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::values::GlobalValue<'c> {
    module.add_global(
        type_::compile(declaration.type_(), context, target_data),
        None,
        declaration.name(),
    )
}

fn compile_function_declaration<'c>(
    module: &inkwell::module::Module<'c>,
    declaration: &FunctionDeclaration,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::values::FunctionValue<'c> {
    let function = module.add_function(
        declaration.name(),
        type_::compile_function(declaration.type_(), context, target_data),
        None,
    );

    function.set_call_conventions(calling_convention::compile(
        declaration.type_().calling_convention(),
    ));

    function
}

fn declare_variable_definition<'c>(
    module: &inkwell::module::Module<'c>,
    definition: &VariableDefinition,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::values::GlobalValue<'c> {
    let global = module.add_global(
        type_::compile(definition.type_(), context, target_data),
        None,
        definition.name(),
    );

    global.set_constant(!definition.is_mutable());
    global.set_linkage(compile_linkage(definition.linkage()));

    if let Some(alignment) = definition.alignment() {
        global.set_alignment(alignment as u32);
    }

    global
}

fn compile_variable_definition<'c>(
    module: &inkwell::module::Module<'c>,
    definition: &VariableDefinition,
    variables: &hamt::Map<String, inkwell::values::BasicValueEnum<'c>>,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) {
    module
        .get_global(definition.name())
        .unwrap()
        .set_initializer(&expression::compile_constant(
            definition.body(),
            variables,
            context,
            target_data,
        ));
}

fn declare_function_definition<'c>(
    module: &inkwell::module::Module<'c>,
    definition: &FunctionDefinition,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::values::FunctionValue<'c> {
    let function = module.add_function(
        definition.name(),
        type_::compile_function(definition.type_(), context, target_data),
        Some(compile_linkage(definition.linkage())),
    );

    function.set_call_conventions(calling_convention::compile(
        definition.type_().calling_convention(),
    ));

    // spell-checker: disable-next-line
    for attribute in ["willreturn", "nounwind"] {
        function.add_attribute(
            inkwell::attributes::AttributeLoc::Function,
            module.get_context().create_enum_attribute(
                inkwell::attributes::Attribute::get_named_enum_kind_id(attribute),
                0,
            ),
        );
    }

    function
        .as_global_value()
        .set_unnamed_address(inkwell::values::UnnamedAddress::Global);

    function
}

fn compile_function_definition<'c>(
    module: &inkwell::module::Module<'c>,
    definition: &FunctionDefinition,
    variables: &hamt::Map<String, inkwell::values::BasicValueEnum<'c>>,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
    instruction_function_set: &InstructionFunctionSet<'c>,
) -> Result<(), CompileError> {
    let function = module.get_function(definition.name()).unwrap();
    let builder = context.create_builder();

    builder.position_at_end(context.append_basic_block(function, "entry"));

    instruction::compile_block(
        &builder,
        definition.body(),
        None,
        &variables.extend(
            definition
                .arguments()
                .iter()
                .enumerate()
                .map(|(index, argument)| {
                    (
                        argument.name().into(),
                        function.get_nth_param(index as u32).unwrap(),
                    )
                }),
        ),
        context,
        target_data,
        instruction_function_set,
    )?;

    function.verify(true);

    Ok(())
}

fn compile_linkage(linkage: fmm::ir::Linkage) -> inkwell::module::Linkage {
    match linkage {
        fmm::ir::Linkage::External => inkwell::module::Linkage::External,
        fmm::ir::Linkage::Internal => inkwell::module::Linkage::Internal,
        fmm::ir::Linkage::Weak => inkwell::module::Linkage::LinkOnceODR,
    }
}

#[cfg(test)]
mod tests {
    use super::{instruction_configuration::DUMMY_INSTRUCTION_CONFIGURATION, *};
    use fmm::types::{self, CallingConvention, Type};

    const POINTER_64_BIT_TARGETS: &[&str] = &[
        "x86_64-unknown-linux-gnu",
        "x86_64-apple-macos",
        "arm64-apple-macos",
    ];
    const DEFAULT_TARGETS: Lazy<Vec<&str>> = Lazy::new(|| {
        POINTER_64_BIT_TARGETS
            .iter()
            .cloned()
            .chain(["i386-unknown-linux-gnu", "wasm32-wasi"])
            .collect()
    });

    fn compile_transformed_module(module: &Module, targets: &[&str]) {
        let one = compile_to_object(module, &DUMMY_INSTRUCTION_CONFIGURATION, None).unwrap();
        let other = compile_to_object(module, &DUMMY_INSTRUCTION_CONFIGURATION, None).unwrap();

        assert_eq!(one, other);

        for target in targets {
            compile_to_object(module, &DUMMY_INSTRUCTION_CONFIGURATION, Some(target)).unwrap();
        }
    }

    fn compile_module_with_targets(module: &Module, targets: &[&str]) {
        compile_transformed_module(module, targets);
        compile_transformed_module(
            &fmm::analysis::transform_to_cps(module, types::void_type()).unwrap(),
            targets,
        );
    }

    fn compile_module(module: &Module) {
        compile_module_with_targets(module, &DEFAULT_TARGETS)
    }

    fn create_function_type(arguments: Vec<Type>, result: impl Into<Type>) -> types::Function {
        types::Function::new(arguments, result, CallingConvention::Source)
    }

    fn create_function_definition(
        name: impl Into<String>,
        arguments: Vec<Argument>,
        body: Block,
        result_type: impl Into<Type>,
        linkage: Linkage,
    ) -> FunctionDefinition {
        FunctionDefinition::new(
            name,
            arguments,
            body,
            result_type,
            CallingConvention::Source,
            linkage,
        )
    }

    fn compile_function_definition(definition: FunctionDefinition) {
        compile_module(&Module::new(vec![], vec![], vec![], vec![definition]));
    }

    #[test]
    fn compile_empty_module() {
        compile_module(&Module::new(vec![], vec![], vec![], vec![]));
    }

    #[test]
    fn compile_for_aarch64() {
        compile_to_object(
            &Module::new(vec![], vec![], vec![], vec![]),
            &DUMMY_INSTRUCTION_CONFIGURATION,
            Some("aarch64-unknown-linux-musl"),
        )
        .unwrap();
    }

    mod variable_declarations {
        use super::*;

        #[test]
        fn compile_pointer_integer() {
            compile_module(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Primitive::PointerInteger,
                )],
                vec![],
                vec![],
                vec![],
            ));
        }

        #[test]
        fn compile_pointer_integer_pointer() {
            compile_module(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                vec![],
                vec![],
                vec![],
            ));
        }

        #[test]
        fn compile_function_pointer() {
            compile_module(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    create_function_type(vec![], types::Primitive::PointerInteger),
                )],
                vec![],
                vec![],
                vec![],
            ));
        }

        #[test]
        fn compile_reference_to_declared_variable() {
            compile_module(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Primitive::PointerInteger,
                )],
                vec![],
                vec![],
                vec![create_function_definition(
                    "f",
                    vec![],
                    Block::new(
                        vec![],
                        Return::new(
                            types::Pointer::new(types::Primitive::PointerInteger),
                            Variable::new("x"),
                        ),
                    ),
                    types::Pointer::new(types::Primitive::PointerInteger),
                    Linkage::External,
                )],
            ));
        }
    }

    mod function_declarations {
        use super::*;

        #[test]
        fn compile_function_pointer() {
            compile_module(&Module::new(
                vec![],
                vec![FunctionDeclaration::new(
                    "x",
                    create_function_type(vec![], types::Primitive::PointerInteger),
                )],
                vec![],
                vec![],
            ));
        }
    }

    mod type_definitions {
        use super::*;

        #[test]
        fn compile_record_type_definition() {
            compile_module(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Record::new(vec![types::Primitive::PointerInteger.into()]),
                )],
                vec![],
                vec![],
                vec![],
            ));
        }

        #[test]
        fn compile_nested_record_type_definition() {
            compile_module(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Record::new(vec![types::Record::new(vec![]).into()]),
                )],
                vec![],
                vec![],
                vec![],
            ));
        }

        #[test]
        fn compile_record_type_definition_with_nested_union_type() {
            compile_module(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Record::new(vec![types::Union::new(vec![
                        types::Primitive::PointerInteger.into(),
                    ])
                    .into()]),
                )],
                vec![],
                vec![],
                vec![],
            ));
        }

        #[test]
        fn compile_union_type_definition() {
            compile_module(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Union::new(vec![types::Primitive::PointerInteger.into()]),
                )],
                vec![],
                vec![],
                vec![],
            ));
        }

        #[test]
        fn compile_nested_union_type_definition() {
            compile_module(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Union::new(vec![types::Union::new(vec![
                        types::Primitive::PointerInteger.into(),
                    ])
                    .into()]),
                )],
                vec![],
                vec![],
                vec![],
            ));
        }

        #[test]
        fn compile_union_type_definition_with_nested_record_type() {
            compile_module(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Union::new(vec![types::Record::new(vec![]).into()]),
                )],
                vec![],
                vec![],
                vec![],
            ));
        }

        #[test]
        fn compile_multiple_record_type_definitions() {
            compile_module(&Module::new(
                vec![
                    VariableDeclaration::new(
                        "x",
                        types::Record::new(vec![types::Primitive::PointerInteger.into()]),
                    ),
                    VariableDeclaration::new(
                        "y",
                        types::Record::new(vec![types::Primitive::Float64.into()]),
                    ),
                    VariableDeclaration::new(
                        "z",
                        types::Record::new(vec![types::Primitive::Boolean.into()]),
                    ),
                ],
                vec![],
                vec![],
                vec![],
            ));
        }
    }

    mod variable_definitions {
        use super::*;

        #[test]
        fn compile_constant_variable() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    fmm::ir::Primitive::PointerInteger(0),
                    types::Primitive::PointerInteger,
                    false,
                    Linkage::External,
                    None,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_mutable_variable() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    fmm::ir::Primitive::PointerInteger(0),
                    types::Primitive::PointerInteger,
                    true,
                    Linkage::External,
                    None,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_internal_variable() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    fmm::ir::Primitive::PointerInteger(0),
                    types::Primitive::PointerInteger,
                    true,
                    Linkage::Internal,
                    None,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_weak_variable() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    fmm::ir::Primitive::PointerInteger(0),
                    types::Primitive::PointerInteger,
                    true,
                    Linkage::Weak,
                    None,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_reference_to_defined_variable() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    fmm::ir::Primitive::PointerInteger(0),
                    types::Primitive::PointerInteger,
                    false,
                    Linkage::External,
                    None,
                )],
                vec![create_function_definition(
                    "f",
                    vec![],
                    Block::new(
                        vec![],
                        Return::new(
                            types::Pointer::new(types::Primitive::PointerInteger),
                            Variable::new("x"),
                        ),
                    ),
                    types::Pointer::new(types::Primitive::PointerInteger),
                    Linkage::External,
                )],
            ));
        }

        #[test]
        fn compile_variable_with_alignment() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    fmm::ir::Primitive::Integer8(0),
                    types::Primitive::Integer8,
                    false,
                    Linkage::External,
                    16,
                )],
                vec![],
            ));
        }
    }

    mod function_definitions {
        use super::*;

        #[test]
        fn compile_external_function() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![],
                vec![create_function_definition(
                    "x",
                    vec![],
                    fmm::ir::Block::new(
                        vec![],
                        fmm::ir::Return::new(
                            types::Primitive::PointerInteger,
                            fmm::ir::Primitive::PointerInteger(0),
                        ),
                    ),
                    types::Primitive::PointerInteger,
                    Linkage::External,
                )],
            ));
        }

        #[test]
        fn compile_internal_function() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![],
                vec![create_function_definition(
                    "x",
                    vec![],
                    fmm::ir::Block::new(
                        vec![],
                        fmm::ir::Return::new(
                            types::Primitive::PointerInteger,
                            fmm::ir::Primitive::PointerInteger(0),
                        ),
                    ),
                    types::Primitive::PointerInteger,
                    Linkage::Internal,
                )],
            ));
        }

        #[test]
        fn fail_to_compile_weak_function() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![],
                vec![create_function_definition(
                    "x",
                    vec![],
                    fmm::ir::Block::new(
                        vec![],
                        fmm::ir::Return::new(
                            types::Primitive::PointerInteger,
                            fmm::ir::Primitive::PointerInteger(0),
                        ),
                    ),
                    types::Primitive::PointerInteger,
                    Linkage::Weak,
                )],
            ));
        }
    }

    mod expressions {
        use super::*;

        #[test]
        fn compile_size_of() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    fmm::ir::SizeOf::new(types::Primitive::Float64),
                    types::Primitive::PointerInteger,
                    false,
                    Linkage::External,
                    None,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_align_of() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    fmm::ir::AlignOf::new(types::Primitive::Float64),
                    types::Primitive::PointerInteger,
                    false,
                    Linkage::External,
                    None,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_bit_cast() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    BitCast::new(
                        types::Primitive::Integer64,
                        types::Primitive::Float64,
                        Primitive::Integer64(42),
                    ),
                    types::Primitive::Float64,
                    false,
                    Linkage::External,
                    None,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_bit_cast_from_pointer() {
            compile_module_with_targets(
                &Module::new(
                    vec![],
                    vec![],
                    vec![
                        VariableDefinition::new(
                            "x",
                            Primitive::Float64(42.0),
                            types::Primitive::Float64,
                            false,
                            Linkage::External,
                            None,
                        ),
                        VariableDefinition::new(
                            "y",
                            BitCast::new(
                                types::Pointer::new(types::Primitive::Float64),
                                types::Primitive::Float64,
                                Variable::new("x"),
                            ),
                            types::Primitive::Float64,
                            false,
                            Linkage::External,
                            None,
                        ),
                    ],
                    vec![],
                ),
                POINTER_64_BIT_TARGETS,
            );
        }

        #[test]
        fn compile_bit_cast_to_pointer() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    BitCast::new(
                        types::Primitive::Float64,
                        types::Pointer::new(types::Primitive::Float64),
                        Primitive::Float64(42.0),
                    ),
                    types::Pointer::new(types::Primitive::Float64),
                    false,
                    Linkage::External,
                    None,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_bit_cast_to_record() {
            let record_type = types::Record::new(vec![types::Primitive::PointerInteger.into()]);

            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![],
                    Return::new(
                        record_type.clone(),
                        BitCast::new(
                            types::Primitive::PointerInteger,
                            record_type.clone(),
                            Primitive::PointerInteger(42),
                        ),
                    ),
                ),
                record_type,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_bit_cast_to_union() {
            let union_type = types::Union::new(vec![types::Primitive::PointerInteger.into()]);

            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![],
                    Return::new(
                        union_type.clone(),
                        BitCast::new(
                            types::Primitive::PointerInteger,
                            union_type.clone(),
                            Primitive::PointerInteger(42),
                        ),
                    ),
                ),
                union_type,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_bitwise_and() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    BitwiseOperation::new(
                        types::Primitive::PointerInteger,
                        BitwiseOperator::And,
                        Primitive::PointerInteger(0),
                        Primitive::PointerInteger(1),
                    ),
                    types::Primitive::PointerInteger,
                    false,
                    Linkage::External,
                    None,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_bitwise_or() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    BitwiseOperation::new(
                        types::Primitive::PointerInteger,
                        BitwiseOperator::Or,
                        Primitive::PointerInteger(0),
                        Primitive::PointerInteger(1),
                    ),
                    types::Primitive::PointerInteger,
                    false,
                    Linkage::External,
                    None,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_bitwise_xor() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    BitwiseOperation::new(
                        types::Primitive::PointerInteger,
                        BitwiseOperator::Xor,
                        Primitive::PointerInteger(0),
                        Primitive::PointerInteger(1),
                    ),
                    types::Primitive::PointerInteger,
                    false,
                    Linkage::External,
                    None,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_bitwise_left_shift() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    BitwiseOperation::new(
                        types::Primitive::PointerInteger,
                        BitwiseOperator::LeftShift,
                        Primitive::PointerInteger(0),
                        Primitive::PointerInteger(1),
                    ),
                    types::Primitive::PointerInteger,
                    false,
                    Linkage::External,
                    None,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_bitwise_right_shift() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    BitwiseOperation::new(
                        types::Primitive::PointerInteger,
                        BitwiseOperator::RightShift(false),
                        Primitive::PointerInteger(0),
                        Primitive::PointerInteger(1),
                    ),
                    types::Primitive::PointerInteger,
                    false,
                    Linkage::External,
                    None,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_signed_bitwise_right_shift() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    BitwiseOperation::new(
                        types::Primitive::PointerInteger,
                        BitwiseOperator::RightShift(true),
                        Primitive::PointerInteger(0),
                        Primitive::PointerInteger(1),
                    ),
                    types::Primitive::PointerInteger,
                    false,
                    Linkage::External,
                    None,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_bitwise_not() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    BitwiseNotOperation::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(0),
                    ),
                    types::Primitive::PointerInteger,
                    false,
                    Linkage::External,
                    None,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_arithmetic_operation() {
            for &operator in &[
                ArithmeticOperator::Add,
                ArithmeticOperator::Subtract,
                ArithmeticOperator::Multiply,
                ArithmeticOperator::Divide,
            ] {
                compile_module(&Module::new(
                    vec![],
                    vec![],
                    vec![VariableDefinition::new(
                        "x",
                        ArithmeticOperation::new(
                            types::Primitive::PointerInteger,
                            operator,
                            Primitive::PointerInteger(1),
                            Primitive::PointerInteger(1),
                        ),
                        types::Primitive::PointerInteger,
                        false,
                        Linkage::External,
                        None,
                    )],
                    vec![],
                ));
            }
        }

        #[test]
        fn compile_comparison_operation() {
            for &operator in &[
                ComparisonOperator::Equal,
                ComparisonOperator::NotEqual,
                ComparisonOperator::LessThan(false),
                ComparisonOperator::LessThan(true),
                ComparisonOperator::GreaterThan(false),
                ComparisonOperator::GreaterThan(true),
                ComparisonOperator::LessThanOrEqual(false),
                ComparisonOperator::LessThanOrEqual(true),
                ComparisonOperator::GreaterThanOrEqual(false),
                ComparisonOperator::GreaterThanOrEqual(true),
            ] {
                compile_module(&Module::new(
                    vec![],
                    vec![],
                    vec![VariableDefinition::new(
                        "x",
                        ComparisonOperation::new(
                            types::Primitive::PointerInteger,
                            operator,
                            Primitive::PointerInteger(1),
                            Primitive::PointerInteger(1),
                        ),
                        types::Primitive::Boolean,
                        false,
                        Linkage::External,
                        None,
                    )],
                    vec![],
                ));
            }
        }

        #[test]
        fn compile_record_address() {
            let record_type = types::Record::new(vec![types::Primitive::PointerInteger.into()]);
            let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

            compile_function_definition(create_function_definition(
                "f",
                vec![Argument::new("x", types::Pointer::new(record_type.clone()))],
                Block::new(
                    vec![],
                    Return::new(
                        pointer_type.clone(),
                        RecordAddress::new(record_type, Variable::new("x"), 0),
                    ),
                ),
                pointer_type,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_record_address_with_global_variable() {
            let record_type = types::Record::new(vec![types::Primitive::PointerInteger.into()]);
            let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

            compile_module(&Module::new(
                vec![VariableDeclaration::new("x", record_type.clone())],
                vec![],
                vec![],
                vec![create_function_definition(
                    "f",
                    vec![],
                    Block::new(
                        vec![],
                        Return::new(
                            pointer_type.clone(),
                            RecordAddress::new(record_type, Variable::new("x"), 0),
                        ),
                    ),
                    pointer_type,
                    Linkage::External,
                )],
            ));
        }

        #[test]
        fn compile_union_address() {
            let union_type = types::Union::new(vec![
                types::Primitive::PointerInteger.into(),
                types::Primitive::Float64.into(),
            ]);
            let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

            compile_function_definition(create_function_definition(
                "f",
                vec![Argument::new("x", types::Pointer::new(union_type.clone()))],
                Block::new(
                    vec![],
                    Return::new(
                        pointer_type.clone(),
                        UnionAddress::new(union_type, Variable::new("x"), 0),
                    ),
                ),
                pointer_type,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_union_address_with_global_variable() {
            let union_type = types::Union::new(vec![
                types::Primitive::PointerInteger.into(),
                types::Primitive::Float64.into(),
            ]);
            let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

            compile_module(&Module::new(
                vec![VariableDeclaration::new("x", union_type.clone())],
                vec![],
                vec![],
                vec![create_function_definition(
                    "f",
                    vec![],
                    Block::new(
                        vec![],
                        Return::new(
                            pointer_type.clone(),
                            UnionAddress::new(union_type, Variable::new("x"), 0),
                        ),
                    ),
                    pointer_type,
                    Linkage::External,
                )],
            ));
        }

        #[test]
        fn compile_constant_pointer_address() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    PointerAddress::new(
                        types::Pointer::new(types::Primitive::Integer8),
                        Undefined::new(types::Pointer::new(types::Primitive::Integer8)),
                        Primitive::PointerInteger(42),
                    ),
                    types::Pointer::new(types::Primitive::Integer8),
                    false,
                    Linkage::External,
                    None,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_constant_record_address() {
            let record_type = types::Record::new(vec![
                types::Primitive::Integer8.into(),
                types::Primitive::PointerInteger.into(),
            ]);

            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    RecordAddress::new(
                        record_type.clone(),
                        Undefined::new(types::Pointer::new(record_type)),
                        1,
                    ),
                    types::Pointer::new(types::Primitive::PointerInteger),
                    false,
                    Linkage::External,
                    None,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_constant_union_address() {
            let union_type = types::Union::new(vec![
                types::Primitive::Integer8.into(),
                types::Primitive::PointerInteger.into(),
            ]);

            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    UnionAddress::new(
                        union_type.clone(),
                        Undefined::new(types::Pointer::new(union_type)),
                        1,
                    ),
                    types::Pointer::new(types::Primitive::PointerInteger),
                    false,
                    Linkage::External,
                    None,
                )],
                vec![],
            ));
        }
    }

    mod instructions {
        use super::*;

        #[test]
        fn compile_call() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![Call::new(
                        types::Function::new(
                            vec![],
                            types::Primitive::PointerInteger,
                            CallingConvention::Source,
                        ),
                        Variable::new("f"),
                        vec![],
                        "x",
                    )
                    .into()],
                    Return::new(types::Primitive::PointerInteger, Variable::new("x")),
                ),
                types::Primitive::PointerInteger,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_tail_call() {
            compile_function_definition(FunctionDefinition::new(
                "f",
                vec![],
                Block::new(
                    vec![Call::new(
                        types::Function::new(
                            vec![],
                            types::Primitive::PointerInteger,
                            CallingConvention::Tail,
                        ),
                        Variable::new("f"),
                        vec![],
                        "x",
                    )
                    .into()],
                    Return::new(types::Primitive::PointerInteger, Variable::new("x")),
                ),
                types::Primitive::PointerInteger,
                CallingConvention::Tail,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_unreachable() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(vec![], TerminalInstruction::Unreachable),
                types::Primitive::PointerInteger,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_unreachable_with_unreachable_function() {
            compile_to_object(
                &Module::new(
                    vec![],
                    vec![],
                    vec![],
                    vec![create_function_definition(
                        "f",
                        vec![],
                        Block::new(vec![], TerminalInstruction::Unreachable),
                        types::Primitive::PointerInteger,
                        Linkage::External,
                    )],
                ),
                &InstructionConfiguration {
                    allocate_function_name: "my_malloc".into(),
                    reallocate_function_name: "my_realloc".into(),
                    free_function_name: "my_free".into(),
                    unreachable_function_name: Some("my_unreachable".into()),
                },
                None,
            )
            .unwrap();
        }

        #[test]
        fn compile_allocate_heap() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![AllocateHeap::new(Primitive::PointerInteger(42), "y").into()],
                    Return::new(types::generic_pointer_type(), Variable::new("y")),
                ),
                types::generic_pointer_type(),
                Linkage::External,
            ));
        }

        #[test]
        fn compile_reallocate_heap() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![
                        AllocateHeap::new(Primitive::PointerInteger(42), "x").into(),
                        ReallocateHeap::new(Variable::new("x"), Primitive::PointerInteger(42), "y")
                            .into(),
                    ],
                    Return::new(types::generic_pointer_type(), Variable::new("y")),
                ),
                types::generic_pointer_type(),
                Linkage::External,
            ));
        }

        #[test]
        fn compile_allocate_stack() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![AllocateStack::new(types::Primitive::PointerInteger, "y").into()],
                    Return::new(
                        types::Pointer::new(types::Primitive::PointerInteger),
                        Variable::new("y"),
                    ),
                ),
                types::Pointer::new(types::Primitive::PointerInteger),
                Linkage::External,
            ));
        }

        #[test]
        fn compile_atomic_load() {
            compile_function_definition(create_function_definition(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                Block::new(
                    vec![AtomicLoad::new(
                        types::Primitive::PointerInteger,
                        Variable::new("x"),
                        AtomicOrdering::Relaxed,
                        "y",
                    )
                    .into()],
                    Return::new(types::Primitive::PointerInteger, Variable::new("y")),
                ),
                types::Primitive::PointerInteger,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_atomic_load_with_function_pointer() {
            let function_type = create_function_type(
                vec![types::Primitive::PointerInteger.into()],
                types::Primitive::PointerInteger,
            );

            compile_function_definition(create_function_definition(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(function_type.clone()),
                )],
                Block::new(
                    vec![AtomicLoad::new(
                        function_type.clone(),
                        Variable::new("x"),
                        AtomicOrdering::Relaxed,
                        "y",
                    )
                    .into()],
                    Return::new(function_type.clone(), Variable::new("y")),
                ),
                function_type,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_atomic_store() {
            compile_function_definition(create_function_definition(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                Block::new(
                    vec![AtomicStore::new(
                        types::Primitive::PointerInteger,
                        Undefined::new(types::Primitive::PointerInteger),
                        Variable::new("x"),
                        AtomicOrdering::Relaxed,
                    )
                    .into()],
                    Return::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(42),
                    ),
                ),
                types::Primitive::PointerInteger,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_atomic_store_with_function_pointer() {
            let function_type = create_function_type(
                vec![types::Primitive::PointerInteger.into()],
                types::Primitive::PointerInteger,
            );

            compile_function_definition(create_function_definition(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(function_type.clone()),
                )],
                Block::new(
                    vec![AtomicStore::new(
                        function_type.clone(),
                        Undefined::new(function_type),
                        Variable::new("x"),
                        AtomicOrdering::Relaxed,
                    )
                    .into()],
                    Return::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(42),
                    ),
                ),
                types::Primitive::PointerInteger,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_fence() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![Fence::new(AtomicOrdering::Release).into()],
                    TerminalInstruction::Unreachable,
                ),
                types::Primitive::PointerInteger,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_if() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![If::new(
                        types::Primitive::PointerInteger,
                        Primitive::Boolean(true),
                        Block::new(
                            vec![],
                            Branch::new(
                                types::Primitive::PointerInteger,
                                Primitive::PointerInteger(42),
                            ),
                        ),
                        Block::new(
                            vec![],
                            Branch::new(
                                types::Primitive::PointerInteger,
                                Primitive::PointerInteger(42),
                            ),
                        ),
                        "x",
                    )
                    .into()],
                    Return::new(types::Primitive::PointerInteger, Variable::new("x")),
                ),
                types::Primitive::PointerInteger,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_if_with_return() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![If::new(
                        types::Primitive::PointerInteger,
                        Primitive::Boolean(true),
                        Block::new(
                            vec![],
                            Return::new(
                                types::Primitive::PointerInteger,
                                Primitive::PointerInteger(42),
                            ),
                        ),
                        Block::new(
                            vec![],
                            Branch::new(
                                types::Primitive::PointerInteger,
                                Primitive::PointerInteger(42),
                            ),
                        ),
                        "x",
                    )
                    .into()],
                    Return::new(types::Primitive::PointerInteger, Variable::new("x")),
                ),
                types::Primitive::PointerInteger,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_if_with_unreachable() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![If::new(
                        types::Primitive::PointerInteger,
                        Primitive::Boolean(true),
                        Block::new(vec![], TerminalInstruction::Unreachable),
                        Block::new(
                            vec![],
                            Branch::new(
                                types::Primitive::PointerInteger,
                                Primitive::PointerInteger(42),
                            ),
                        ),
                        "x",
                    )
                    .into()],
                    Return::new(types::Primitive::PointerInteger, Variable::new("x")),
                ),
                types::Primitive::PointerInteger,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_if_with_expression_generating_instructions() {
            let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![If::new(
                        pointer_type.clone(),
                        Primitive::Boolean(true),
                        Block::new(
                            vec![],
                            Branch::new(
                                pointer_type.clone(),
                                BitCast::new(
                                    types::Pointer::new(types::Primitive::Float64),
                                    pointer_type.clone(),
                                    Undefined::new(types::Pointer::new(types::Primitive::Float64)),
                                ),
                            ),
                        ),
                        Block::new(vec![], TerminalInstruction::Unreachable),
                        "x",
                    )
                    .into()],
                    Return::new(pointer_type.clone(), Variable::new("x")),
                ),
                pointer_type,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_deconstruct_record() {
            let record_type = types::Record::new(vec![types::Primitive::PointerInteger.into()]);

            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![DeconstructRecord::new(
                        record_type.clone(),
                        Record::new(record_type, vec![Primitive::PointerInteger(42).into()]),
                        0,
                        "x",
                    )
                    .into()],
                    Return::new(types::Primitive::PointerInteger, Variable::new("x")),
                ),
                types::Primitive::PointerInteger,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_deconstruct_union() {
            let union_type = types::Union::new(vec![types::Primitive::PointerInteger.into()]);

            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![DeconstructUnion::new(
                        union_type.clone(),
                        Union::new(union_type, 0, Primitive::PointerInteger(42)),
                        0,
                        "x",
                    )
                    .into()],
                    Return::new(types::Primitive::PointerInteger, Variable::new("x")),
                ),
                types::Primitive::PointerInteger,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_compare_and_swap() {
            compile_function_definition(create_function_definition(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                Block::new(
                    vec![CompareAndSwap::new(
                        types::Primitive::PointerInteger,
                        Variable::new("x"),
                        Primitive::PointerInteger(0),
                        Primitive::PointerInteger(1),
                        AtomicOrdering::Relaxed,
                        AtomicOrdering::Relaxed,
                        "y",
                    )
                    .into()],
                    Return::new(types::Primitive::Boolean, Variable::new("y")),
                ),
                types::Primitive::Boolean,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_atomic_add() {
            compile_function_definition(create_function_definition(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                Block::new(
                    vec![AtomicOperation::new(
                        types::Primitive::PointerInteger,
                        AtomicOperator::Add,
                        Variable::new("x"),
                        Primitive::PointerInteger(42),
                        AtomicOrdering::Relaxed,
                        "y",
                    )
                    .into()],
                    Return::new(types::Primitive::PointerInteger, Variable::new("y")),
                ),
                types::Primitive::PointerInteger,
                Linkage::External,
            ));
        }

        #[test]
        fn compile_atomic_operation_with_different_ordering() {
            for &ordering in &[
                AtomicOrdering::Relaxed,
                AtomicOrdering::Release,
                AtomicOrdering::Acquire,
                AtomicOrdering::AcquireRelease,
                AtomicOrdering::SequentiallyConsistent,
            ] {
                compile_function_definition(create_function_definition(
                    "f",
                    vec![Argument::new(
                        "x",
                        types::Pointer::new(types::Primitive::PointerInteger),
                    )],
                    Block::new(
                        vec![AtomicOperation::new(
                            types::Primitive::PointerInteger,
                            AtomicOperator::Add,
                            Variable::new("x"),
                            Primitive::PointerInteger(42),
                            ordering,
                            "y",
                        )
                        .into()],
                        Return::new(types::Primitive::PointerInteger, Variable::new("y")),
                    ),
                    types::Primitive::PointerInteger,
                    Linkage::External,
                ));
            }
        }

        #[test]
        fn compile_free_heap() {
            compile_function_definition(create_function_definition(
                "f",
                vec![Argument::new("x", types::generic_pointer_type())],
                Block::new(
                    vec![FreeHeap::new(Variable::new("x")).into()],
                    Return::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(0),
                    ),
                ),
                types::Primitive::PointerInteger,
                Linkage::External,
            ));
        }
    }
}
