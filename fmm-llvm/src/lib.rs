mod calling_convention;
mod context;
mod error;
mod expression;
mod instruction;
mod instruction_configuration;
mod type_;
mod union;

use context::Context;
pub use error::CompileError;
use fmm::ir::*;
pub use instruction_configuration::InstructionConfiguration;
use instruction_configuration::InstructionFunctionSet;

pub fn compile_to_bit_code(
    module: &Module,
    instruction_configuration: &InstructionConfiguration,
    target_triple: Option<&str>,
) -> Result<Vec<u8>, CompileError> {
    let context = Context::new(target_triple, instruction_configuration.clone())?;
    let module = compile_module(&context, module)?;

    Ok(module.write_bitcode_to_memory().as_slice().to_vec())
}

pub fn compile_to_object(
    module: &Module,
    instruction_configuration: &InstructionConfiguration,
    target_triple: Option<&str>,
) -> Result<Vec<u8>, CompileError> {
    let context = Context::new(target_triple, instruction_configuration.clone())?;
    let module = compile_module(&context, module)?;

    // TODO How can I set something equivalent to llvm::GuaranteedTailCallOpt in
    // C++? https://llvm.org/docs/LangRef.html#call-instruction
    Ok(context
        .target_machine()
        .write_to_memory_buffer(&module, inkwell::targets::FileType::Object)?
        .as_slice()
        .to_vec())
}

fn compile_module<'c>(
    context: &'c Context,
    module: &Module,
) -> Result<inkwell::module::Module<'c>, CompileError> {
    fmm::analysis::name::check(module)?;
    fmm::analysis::check_types(module)?;

    let llvm_module = context.inkwell().create_module("");
    llvm_module.set_triple(&context.target_machine().get_triple());

    let mut variables = hamt::Map::new();

    let instruction_function_set = compile_heap_functions(context, &llvm_module);

    for declaration in module.variable_declarations() {
        let global = compile_variable_declaration(context, &llvm_module, declaration);

        variables = variables.insert(declaration.name(), global.as_pointer_value().into());
    }

    for declaration in module.function_declarations() {
        let function = compile_function_declaration(context, &llvm_module, declaration);

        variables = variables.insert(
            declaration.name(),
            function.as_global_value().as_pointer_value().into(),
        );
    }

    for definition in module.variable_definitions() {
        let global = declare_variable_definition(context, &llvm_module, definition);

        variables = variables.insert(definition.name(), global.as_pointer_value().into());
    }

    for definition in module.function_definitions() {
        let function = declare_function_definition(context, &llvm_module, definition);

        variables = variables.insert(
            definition.name(),
            function.as_global_value().as_pointer_value().into(),
        );
    }

    for definition in module.variable_definitions() {
        compile_variable_definition(context, &llvm_module, definition, &variables);
    }

    for definition in module.function_definitions() {
        compile_function_definition(
            context,
            &llvm_module,
            definition,
            &variables,
            &instruction_function_set,
        )?;
    }

    llvm_module.verify()?;

    Ok(llvm_module)
}

fn compile_heap_functions<'c>(
    context: &'c Context,
    module: &inkwell::module::Module<'c>,
) -> InstructionFunctionSet<'c> {
    let pointer_type = context
        .inkwell()
        .i8_type()
        .ptr_type(type_::DEFAULT_ADDRESS_SPACE);
    let pointer_integer_type = type_::compile_pointer_integer(context);

    InstructionFunctionSet {
        allocate_function: module.add_function(
            &context.instruction_configuration().allocate_function_name,
            pointer_type.fn_type(&[pointer_integer_type.into()], false),
            None,
        ),
        reallocate_function: module.add_function(
            &context.instruction_configuration().reallocate_function_name,
            pointer_type.fn_type(&[pointer_type.into(), pointer_integer_type.into()], false),
            None,
        ),
        free_function: module.add_function(
            &context.instruction_configuration().free_function_name,
            context
                .inkwell()
                .void_type()
                .fn_type(&[pointer_type.into()], false),
            None,
        ),
        unreachable_function: context
            .instruction_configuration()
            .unreachable_function_name
            .as_ref()
            .map(|name| {
                module.add_function(
                    name,
                    context.inkwell().void_type().fn_type(&[], false),
                    None,
                )
            }),
    }
}

fn compile_variable_declaration<'c>(
    context: &'c Context,
    module: &inkwell::module::Module<'c>,
    declaration: &VariableDeclaration,
) -> inkwell::values::GlobalValue<'c> {
    module.add_global(
        type_::compile(context, declaration.type_()),
        None,
        declaration.name(),
    )
}

fn compile_function_declaration<'c>(
    context: &'c Context,
    module: &inkwell::module::Module<'c>,
    declaration: &FunctionDeclaration,
) -> inkwell::values::FunctionValue<'c> {
    let function = module.add_function(
        declaration.name(),
        type_::compile_function(context, declaration.type_()),
        None,
    );

    function.set_call_conventions(calling_convention::compile(
        declaration.type_().calling_convention(),
    ));

    function
}

fn declare_variable_definition<'c>(
    context: &'c Context,
    module: &inkwell::module::Module<'c>,
    definition: &VariableDefinition,
) -> inkwell::values::GlobalValue<'c> {
    let global = module.get_global(definition.name()).unwrap_or_else(|| {
        module.add_global(
            type_::compile(context, definition.type_()),
            None,
            definition.name(),
        )
    });

    global.set_constant(!definition.options().is_mutable());
    global.set_linkage(compile_linkage(definition.options().linkage()));
    global.set_unnamed_address(compiled_address_named(
        definition.options().is_address_named(),
    ));

    if let Some(alignment) = definition.options().alignment() {
        global.set_alignment(alignment as u32);
    }

    global
}

fn compile_variable_definition<'c>(
    context: &'c Context,
    module: &inkwell::module::Module<'c>,
    definition: &VariableDefinition,
    variables: &hamt::Map<&str, inkwell::values::BasicValueEnum<'c>>,
) {
    module
        .get_global(definition.name())
        .unwrap()
        .set_initializer(&expression::compile_constant(
            context,
            definition.body(),
            variables,
        ));
}

fn declare_function_definition<'c>(
    context: &'c Context,
    module: &inkwell::module::Module<'c>,
    definition: &FunctionDefinition,
) -> inkwell::values::FunctionValue<'c> {
    let function = module.get_function(definition.name()).unwrap_or_else(|| {
        module.add_function(
            definition.name(),
            type_::compile_function(context, definition.type_()),
            Some(compile_linkage(definition.options().linkage())),
        )
    });

    function.set_call_conventions(calling_convention::compile(
        definition.type_().calling_convention(),
    ));
    function
        .as_global_value()
        .set_unnamed_address(compiled_address_named(
            definition.options().is_address_named(),
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
}

fn compile_function_definition<'c>(
    context: &'c Context,
    module: &inkwell::module::Module<'c>,
    definition: &FunctionDefinition,
    variables: &hamt::Map<&str, inkwell::values::BasicValueEnum<'c>>,
    instruction_function_set: &InstructionFunctionSet<'c>,
) -> Result<(), CompileError> {
    let function = module.get_function(definition.name()).unwrap();
    let builder = context.inkwell().create_builder();

    builder.position_at_end(context.inkwell().append_basic_block(function, "entry"));

    instruction::compile_block(
        context,
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
                        argument.name(),
                        function.get_nth_param(index as u32).unwrap(),
                    )
                }),
        ),
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

fn compiled_address_named(address_named: bool) -> inkwell::values::UnnamedAddress {
    if address_named {
        inkwell::values::UnnamedAddress::None
    } else {
        inkwell::values::UnnamedAddress::Global
    }
}

#[cfg(test)]
mod tests {
    use super::{instruction_configuration::DUMMY_INSTRUCTION_CONFIGURATION, *};
    use fmm::types::{self, CallingConvention, Type};
    use once_cell::sync::Lazy;

    const POINTER_64_BIT_TARGETS: &[&str] = &[
        "x86_64-unknown-linux-gnu",
        "x86_64-apple-macos",
        "arm64-apple-macos",
    ];
    static DEFAULT_TARGETS: Lazy<Vec<&str>> = Lazy::new(|| {
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
        result_type: impl Into<Type>,
        body: Block,
    ) -> FunctionDefinition {
        FunctionDefinition::new(name, arguments, result_type, body, Default::default())
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
                    types::Pointer::new(types::Primitive::PointerInteger),
                    Block::new(
                        vec![],
                        Return::new(
                            types::Pointer::new(types::Primitive::PointerInteger),
                            Variable::new("x"),
                        ),
                    ),
                )],
            ));
        }

        #[test]
        fn check_variable_declaration_matching_definition() {
            compile_module(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Primitive::PointerInteger,
                )],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    Primitive::PointerInteger(42),
                    types::Primitive::PointerInteger,
                    Default::default(),
                )],
                vec![],
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

        #[test]
        fn check_function_declaration_matching_definition() {
            compile_module(&Module::new(
                vec![],
                vec![FunctionDeclaration::new(
                    "f",
                    types::Function::new(
                        vec![],
                        types::Primitive::PointerInteger,
                        CallingConvention::Source,
                    ),
                )],
                vec![],
                vec![create_function_definition(
                    "f",
                    vec![],
                    types::Primitive::PointerInteger,
                    Block::new(vec![], TerminalInstruction::Unreachable),
                )],
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
                    VariableDefinitionOptions::new().set_mutable(false),
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
                    VariableDefinitionOptions::new().set_mutable(true),
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_external_variable() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    fmm::ir::Primitive::PointerInteger(0),
                    types::Primitive::PointerInteger,
                    VariableDefinitionOptions::new().set_linkage(Linkage::External),
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
                    VariableDefinitionOptions::new().set_linkage(Linkage::Internal),
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
                    VariableDefinitionOptions::new().set_linkage(Linkage::Weak),
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_variable_with_address_named() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    fmm::ir::Primitive::PointerInteger(0),
                    types::Primitive::PointerInteger,
                    VariableDefinitionOptions::new().set_address_named(true),
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_variable_with_address_unnamed() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    fmm::ir::Primitive::PointerInteger(0),
                    types::Primitive::PointerInteger,
                    VariableDefinitionOptions::new().set_address_named(false),
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_reference_to_variable() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    fmm::ir::Primitive::PointerInteger(0),
                    types::Primitive::PointerInteger,
                    Default::default(),
                )],
                vec![create_function_definition(
                    "f",
                    vec![],
                    types::Pointer::new(types::Primitive::PointerInteger),
                    Block::new(
                        vec![],
                        Return::new(
                            types::Pointer::new(types::Primitive::PointerInteger),
                            Variable::new("x"),
                        ),
                    ),
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
                    VariableDefinitionOptions::new().set_alignment(Some(16)),
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
                vec![FunctionDefinition::new(
                    "x",
                    vec![],
                    types::Primitive::PointerInteger,
                    fmm::ir::Block::new(
                        vec![],
                        fmm::ir::Return::new(
                            types::Primitive::PointerInteger,
                            fmm::ir::Primitive::PointerInteger(0),
                        ),
                    ),
                    FunctionDefinitionOptions::new().set_linkage(Linkage::External),
                )],
            ));
        }

        #[test]
        fn compile_internal_function() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "x",
                    vec![],
                    types::Primitive::PointerInteger,
                    fmm::ir::Block::new(
                        vec![],
                        fmm::ir::Return::new(
                            types::Primitive::PointerInteger,
                            fmm::ir::Primitive::PointerInteger(0),
                        ),
                    ),
                    FunctionDefinitionOptions::new().set_linkage(Linkage::Internal),
                )],
            ));
        }

        #[test]
        fn fail_to_compile_weak_function() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "x",
                    vec![],
                    types::Primitive::PointerInteger,
                    fmm::ir::Block::new(
                        vec![],
                        fmm::ir::Return::new(
                            types::Primitive::PointerInteger,
                            fmm::ir::Primitive::PointerInteger(0),
                        ),
                    ),
                    FunctionDefinitionOptions::new().set_linkage(Linkage::Weak),
                )],
            ));
        }

        #[test]
        fn compile_function_with_address_named() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "x",
                    vec![],
                    types::Primitive::PointerInteger,
                    fmm::ir::Block::new(
                        vec![],
                        fmm::ir::Return::new(
                            types::Primitive::PointerInteger,
                            fmm::ir::Primitive::PointerInteger(0),
                        ),
                    ),
                    FunctionDefinitionOptions::new().set_address_named(true),
                )],
            ));
        }

        #[test]
        fn compile_function_with_address_unnamed() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "x",
                    vec![],
                    types::Primitive::PointerInteger,
                    fmm::ir::Block::new(
                        vec![],
                        fmm::ir::Return::new(
                            types::Primitive::PointerInteger,
                            fmm::ir::Primitive::PointerInteger(0),
                        ),
                    ),
                    FunctionDefinitionOptions::new().set_address_named(false),
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
                    Default::default(),
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
                    Default::default(),
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
                    Default::default(),
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
                            Default::default(),
                        ),
                        VariableDefinition::new(
                            "y",
                            BitCast::new(
                                types::Pointer::new(types::Primitive::Float64),
                                types::Primitive::Float64,
                                Variable::new("x"),
                            ),
                            types::Primitive::Float64,
                            Default::default(),
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
                    Default::default(),
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
                record_type.clone(),
                Block::new(
                    vec![],
                    Return::new(
                        record_type.clone(),
                        BitCast::new(
                            types::Primitive::PointerInteger,
                            record_type,
                            Primitive::PointerInteger(42),
                        ),
                    ),
                ),
            ));
        }

        #[test]
        fn compile_bit_cast_to_union() {
            let union_type = types::Union::new(vec![types::Primitive::PointerInteger.into()]);

            compile_function_definition(create_function_definition(
                "f",
                vec![],
                union_type.clone(),
                Block::new(
                    vec![],
                    Return::new(
                        union_type.clone(),
                        BitCast::new(
                            types::Primitive::PointerInteger,
                            union_type,
                            Primitive::PointerInteger(42),
                        ),
                    ),
                ),
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
                    Default::default(),
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
                    Default::default(),
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
                    Default::default(),
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
                    Default::default(),
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
                    Default::default(),
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
                    Default::default(),
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
                    Default::default(),
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
                        Default::default(),
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
                        Default::default(),
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
                pointer_type.clone(),
                Block::new(
                    vec![],
                    Return::new(
                        pointer_type,
                        RecordAddress::new(record_type, Variable::new("x"), 0),
                    ),
                ),
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
                    pointer_type.clone(),
                    Block::new(
                        vec![],
                        Return::new(
                            pointer_type,
                            RecordAddress::new(record_type, Variable::new("x"), 0),
                        ),
                    ),
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
                pointer_type.clone(),
                Block::new(
                    vec![],
                    Return::new(
                        pointer_type,
                        UnionAddress::new(union_type, Variable::new("x"), 0),
                    ),
                ),
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
                    pointer_type.clone(),
                    Block::new(
                        vec![],
                        Return::new(
                            pointer_type,
                            UnionAddress::new(union_type, Variable::new("x"), 0),
                        ),
                    ),
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
                    Default::default(),
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
                    Default::default(),
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
                    Default::default(),
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
                types::Primitive::PointerInteger,
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
            ));
        }

        #[test]
        fn compile_tail_call() {
            compile_function_definition(FunctionDefinition::new(
                "f",
                vec![],
                types::Primitive::PointerInteger,
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
                FunctionDefinitionOptions::new().set_calling_convention(CallingConvention::Tail),
            ));
        }

        #[test]
        fn compile_unreachable() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                types::Primitive::PointerInteger,
                Block::new(vec![], TerminalInstruction::Unreachable),
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
                        types::Primitive::PointerInteger,
                        Block::new(vec![], TerminalInstruction::Unreachable),
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
                types::generic_pointer_type(),
                Block::new(
                    vec![AllocateHeap::new(Primitive::PointerInteger(42), "y").into()],
                    Return::new(types::generic_pointer_type(), Variable::new("y")),
                ),
            ));
        }

        #[test]
        fn compile_reallocate_heap() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                types::generic_pointer_type(),
                Block::new(
                    vec![
                        AllocateHeap::new(Primitive::PointerInteger(42), "x").into(),
                        ReallocateHeap::new(Variable::new("x"), Primitive::PointerInteger(42), "y")
                            .into(),
                    ],
                    Return::new(types::generic_pointer_type(), Variable::new("y")),
                ),
            ));
        }

        #[test]
        fn compile_allocate_stack() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                types::Pointer::new(types::Primitive::PointerInteger),
                Block::new(
                    vec![AllocateStack::new(types::Primitive::PointerInteger, "y").into()],
                    Return::new(
                        types::Pointer::new(types::Primitive::PointerInteger),
                        Variable::new("y"),
                    ),
                ),
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
                types::Primitive::PointerInteger,
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
                function_type.clone(),
                Block::new(
                    vec![AtomicLoad::new(
                        function_type.clone(),
                        Variable::new("x"),
                        AtomicOrdering::Relaxed,
                        "y",
                    )
                    .into()],
                    Return::new(function_type, Variable::new("y")),
                ),
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
                types::Primitive::PointerInteger,
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
                types::Primitive::PointerInteger,
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
            ));
        }

        #[test]
        fn compile_fence() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![Fence::new(AtomicOrdering::Release).into()],
                    TerminalInstruction::Unreachable,
                ),
            ));
        }

        #[test]
        fn compile_if() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                types::Primitive::PointerInteger,
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
            ));
        }

        #[test]
        fn compile_if_with_return() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                types::Primitive::PointerInteger,
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
            ));
        }

        #[test]
        fn compile_if_with_unreachable() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                types::Primitive::PointerInteger,
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
            ));
        }

        #[test]
        fn compile_if_with_expression_generating_instructions() {
            let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

            compile_function_definition(create_function_definition(
                "f",
                vec![],
                pointer_type.clone(),
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
                    Return::new(pointer_type, Variable::new("x")),
                ),
            ));
        }

        #[test]
        fn compile_deconstruct_record() {
            let record_type = types::Record::new(vec![types::Primitive::PointerInteger.into()]);

            compile_function_definition(create_function_definition(
                "f",
                vec![],
                types::Primitive::PointerInteger,
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
            ));
        }

        #[test]
        fn compile_deconstruct_union() {
            let union_type = types::Union::new(vec![types::Primitive::PointerInteger.into()]);

            compile_function_definition(create_function_definition(
                "f",
                vec![],
                types::Primitive::PointerInteger,
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
                types::Primitive::Boolean,
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
                types::Primitive::PointerInteger,
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
                    types::Primitive::PointerInteger,
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
                ));
            }
        }

        #[test]
        fn compile_free_heap() {
            compile_function_definition(create_function_definition(
                "f",
                vec![Argument::new("x", types::generic_pointer_type())],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![FreeHeap::new(Variable::new("x")).into()],
                    Return::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(0),
                    ),
                ),
            ));
        }
    }
}
