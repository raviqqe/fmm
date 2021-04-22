mod calling_convention;
mod error;
mod expressions;
mod heap;
mod instructions;
mod types;
mod union;

use calling_convention::*;
use error::CompileError;
use expressions::*;
use fmm::ir::*;
pub use heap::HeapConfiguration;
use heap::HeapFunctionSet;
use instructions::*;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use types::*;

static DEFAULT_TARGET_TRIPLE: Lazy<String> = Lazy::new(|| {
    inkwell::targets::TargetMachine::get_default_triple()
        .as_str()
        .to_str()
        .unwrap()
        .into()
});

pub fn compile_to_bitcode(
    module: &Module,
    heap_configuration: &HeapConfiguration,
    target_triple: Option<&str>,
) -> Result<Vec<u8>, CompileError> {
    let target_machine = create_target_machine(target_triple)?;
    let context = inkwell::context::Context::create();

    let module = compile_module(&context, &target_machine, module, heap_configuration)?;

    Ok(module.write_bitcode_to_memory().as_slice().to_vec())
}

pub fn compile_to_object(
    module: &Module,
    heap_configuration: &HeapConfiguration,
    target_triple: Option<&str>,
) -> Result<Vec<u8>, CompileError> {
    let target_machine = create_target_machine(target_triple)?;
    let context = inkwell::context::Context::create();

    let module = compile_module(&context, &target_machine, module, heap_configuration)?;

    // TODO How can I set something equivalent to llvm::GuaranteedTailCallOpt in C++?
    // https://llvm.org/docs/LangRef.html#call-instruction
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
            inkwell::targets::CodeModel::Medium,
        )
        .ok_or(CompileError::TargetMachineNotCreated)
}

fn compile_module<'c>(
    context: &'c inkwell::context::Context,
    target_machine: &inkwell::targets::TargetMachine,
    module: &Module,
    heap_configuration: &HeapConfiguration,
) -> Result<inkwell::module::Module<'c>, CompileError> {
    let target_data = target_machine.get_target_data();

    let llvm_module = context.create_module("");
    llvm_module.set_triple(&target_machine.get_triple());

    let mut variables = HashMap::new();

    let heap_function_set =
        compile_heap_functions(&llvm_module, heap_configuration, &context, &target_data);

    for declaration in module.variable_declarations() {
        let global =
            compile_variable_declaration(&llvm_module, declaration, &context, &target_data);

        variables.insert(declaration.name().into(), global.as_pointer_value().into());
    }

    for declaration in module.function_declarations() {
        let function =
            compile_function_declaration(&llvm_module, declaration, &context, &target_data);

        variables.insert(
            declaration.name().into(),
            function.as_global_value().as_pointer_value().into(),
        );
    }

    for definition in module.variable_definitions() {
        let global = declare_variable_definition(&llvm_module, definition, &context, &target_data);

        variables.insert(definition.name().into(), global.as_pointer_value().into());
    }

    for definition in module.function_definitions() {
        let function =
            declare_function_definition(&llvm_module, definition, &context, &target_data);

        variables.insert(
            definition.name().into(),
            function.as_global_value().as_pointer_value().into(),
        );
    }

    for definition in module.variable_definitions() {
        compile_variable_definition(&llvm_module, definition, &variables, &context, &target_data);
    }

    for definition in module.function_definitions() {
        compile_function_definition(
            &llvm_module,
            definition,
            &variables,
            &context,
            &target_data,
            &heap_function_set,
        );
    }

    llvm_module.verify()?;

    Ok(llvm_module)
}

fn compile_heap_functions<'c>(
    module: &inkwell::module::Module<'c>,
    heap_configuration: &HeapConfiguration,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> HeapFunctionSet<'c> {
    let pointer_type = context.i8_type().ptr_type(DEFAULT_ADDRESS_SPACE);
    let pointer_integer_type = compile_pointer_integer_type(context, target_data);

    HeapFunctionSet {
        allocate_function: module.add_function(
            &heap_configuration.allocate_function_name,
            pointer_type.fn_type(&[pointer_integer_type.into()], false),
            None,
        ),
        reallocate_function: module.add_function(
            &heap_configuration.reallocate_function_name,
            pointer_type.fn_type(&[pointer_type.into(), pointer_integer_type.into()], false),
            None,
        ),
        free_function: module.add_function(
            &heap_configuration.free_function_name,
            context.void_type().fn_type(&[pointer_type.into()], false),
            None,
        ),
    }
}

fn compile_variable_declaration<'c>(
    module: &inkwell::module::Module<'c>,
    declaration: &VariableDeclaration,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) -> inkwell::values::GlobalValue<'c> {
    module.add_global(
        compile_type(declaration.type_(), context, target_data),
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
        compile_function_type(declaration.type_(), context, target_data),
        None,
    );

    function.set_call_conventions(compile_calling_convention(
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
        compile_type(definition.type_(), context, target_data),
        None,
        definition.name(),
    );

    global.set_constant(!definition.is_mutable());
    global.set_linkage(compile_linkage(definition.is_global()));

    global
}

fn compile_variable_definition<'c>(
    module: &inkwell::module::Module<'c>,
    definition: &VariableDefinition,
    variables: &HashMap<String, inkwell::values::BasicValueEnum<'c>>,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) {
    module
        .get_global(definition.name())
        .unwrap()
        .set_initializer(&compile_constant_expression(
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
        compile_function_type(definition.type_(), context, target_data),
        Some(compile_linkage(definition.is_global())),
    );

    function.set_call_conventions(compile_calling_convention(
        definition.type_().calling_convention(),
    ));

    function
}

fn compile_function_definition<'c>(
    module: &inkwell::module::Module<'c>,
    definition: &FunctionDefinition,
    variables: &HashMap<String, inkwell::values::BasicValueEnum<'c>>,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
    heap_function_set: &HeapFunctionSet<'c>,
) {
    let function = module.get_function(definition.name()).unwrap();
    let builder = context.create_builder();

    builder.position_at_end(context.append_basic_block(function, "entry"));

    compile_block(
        &builder,
        definition.body(),
        None,
        &variables
            .clone()
            .into_iter()
            .chain(
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
            )
            .collect(),
        context,
        target_data,
        heap_function_set,
    );

    function.verify(true);
}

fn compile_linkage(is_global: bool) -> inkwell::module::Linkage {
    if is_global {
        inkwell::module::Linkage::External
    } else {
        inkwell::module::Linkage::Private
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fmm::types::{self, CallingConvention, Type};

    fn compile_final_module(module: &Module) {
        fmm::analysis::check_types(module).unwrap();

        compile_to_object(
            module,
            &HeapConfiguration {
                allocate_function_name: "my_malloc".into(),
                reallocate_function_name: "my_realloc".into(),
                free_function_name: "my_free".into(),
            },
            None,
        )
        .unwrap();
    }

    fn compile_module(module: &Module) {
        compile_final_module(module);
        compile_final_module(
            &fmm::analysis::transform_to_cps(module, types::Record::new(vec![])).unwrap(),
        );
    }

    fn create_function_type(arguments: Vec<Type>, result: impl Into<Type>) -> types::Function {
        types::Function::new(arguments, result, CallingConvention::Source)
    }

    fn create_function_definition(
        name: impl Into<String>,
        arguments: Vec<Argument>,
        body: Block,
        result_type: impl Into<Type>,
        global: bool,
    ) -> FunctionDefinition {
        FunctionDefinition::new(
            name,
            arguments,
            body,
            result_type,
            CallingConvention::Source,
            global,
        )
    }

    fn compile_function_definition(definition: FunctionDefinition) {
        compile_module(&Module::new(vec![], vec![], vec![], vec![definition]));
    }

    #[test]
    fn compile_empty_module() {
        compile_module(&Module::new(vec![], vec![], vec![], vec![]));
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
                    false,
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
                    true,
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
                    true,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_local_variable() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    fmm::ir::Primitive::PointerInteger(0),
                    types::Primitive::PointerInteger,
                    true,
                    false,
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
                    false,
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
                    false,
                )],
            ));
        }
    }

    mod function_definitions {
        use super::*;

        #[test]
        fn compile_global_function() {
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
                    true,
                )],
            ));
        }

        #[test]
        fn compile_local_function() {
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
                    false,
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
                    false,
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
                    false,
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
                    true,
                )],
                vec![],
            ));
        }

        #[test]
        fn compile_bit_cast_from_pointer() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![
                    VariableDefinition::new(
                        "x",
                        Primitive::Float64(42.0),
                        types::Primitive::Float64,
                        false,
                        false,
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
                        false,
                    ),
                ],
                vec![],
            ));
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
                    false,
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
                record_type.clone(),
                false,
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
                union_type.clone(),
                false,
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
                    false,
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
                    false,
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
                    false,
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
                        false,
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
                ComparisonOperator::LessThan,
                ComparisonOperator::GreaterThan,
                ComparisonOperator::LessThanOrEqual,
                ComparisonOperator::GreaterThanOrEqual,
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
                        false,
                    )],
                    vec![],
                ));
            }
        }
    }

    mod instructions {
        use super::*;

        #[test]
        fn compile_unreachable() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(vec![], TerminalInstruction::Unreachable),
                types::Primitive::PointerInteger,
                true,
            ));
        }

        #[test]
        fn compile_allocate_heap() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![AllocateHeap::new(types::Primitive::PointerInteger, "y").into()],
                    Return::new(
                        types::Pointer::new(types::Primitive::PointerInteger),
                        Variable::new("y"),
                    ),
                ),
                types::Pointer::new(types::Primitive::PointerInteger),
                true,
            ));
        }

        #[test]
        fn compile_reallocate_heap() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![
                        AllocateHeap::new(types::Primitive::Integer8, "x").into(),
                        ReallocateHeap::new(Variable::new("x"), Primitive::PointerInteger(42), "y")
                            .into(),
                    ],
                    Return::new(
                        types::Pointer::new(types::Primitive::Integer8),
                        Variable::new("y"),
                    ),
                ),
                types::Pointer::new(types::Primitive::Integer8),
                true,
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
                true,
            ));
        }

        #[test]
        fn compile_allocate_heap_with_function_pointer() {
            let function_type = create_function_type(
                vec![types::Primitive::PointerInteger.into()],
                types::Primitive::PointerInteger,
            );

            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![AllocateHeap::new(function_type.clone(), "y").into()],
                    Return::new(
                        types::Pointer::new(function_type.clone()),
                        Variable::new("y"),
                    ),
                ),
                types::Pointer::new(function_type),
                true,
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
                        "y",
                    )
                    .into()],
                    Return::new(types::Primitive::PointerInteger, Variable::new("y")),
                ),
                types::Primitive::PointerInteger,
                true,
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
                    vec![AtomicLoad::new(function_type.clone(), Variable::new("x"), "y").into()],
                    Return::new(function_type.clone(), Variable::new("y")),
                ),
                function_type,
                true,
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
                    )
                    .into()],
                    Return::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(42),
                    ),
                ),
                types::Primitive::PointerInteger,
                true,
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
                    )
                    .into()],
                    Return::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(42),
                    ),
                ),
                types::Primitive::PointerInteger,
                true,
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
                true,
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
                true,
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
                true,
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
                true,
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
                true,
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
                true,
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
                        "y",
                    )
                    .into()],
                    Return::new(types::Primitive::Boolean, Variable::new("y")),
                ),
                types::Primitive::Boolean,
                true,
            ));
        }

        #[test]
        fn compile_record_address() {
            let record_type = types::Record::new(vec![types::Primitive::PointerInteger.into()]);
            let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

            compile_function_definition(create_function_definition(
                "f",
                vec![Argument::new("x", types::Pointer::new(record_type.clone()))],
                Block::new(
                    vec![RecordAddress::new(record_type, Variable::new("x"), 0, "y").into()],
                    Return::new(pointer_type.clone(), Variable::new("y")),
                ),
                pointer_type,
                true,
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
                        vec![RecordAddress::new(record_type, Variable::new("x"), 0, "y").into()],
                        Return::new(pointer_type.clone(), Variable::new("y")),
                    ),
                    pointer_type,
                    true,
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
                    vec![UnionAddress::new(union_type, Variable::new("x"), 0, "y").into()],
                    Return::new(pointer_type.clone(), Variable::new("y")),
                ),
                pointer_type,
                true,
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
                        vec![UnionAddress::new(union_type, Variable::new("x"), 0, "y").into()],
                        Return::new(pointer_type.clone(), Variable::new("y")),
                    ),
                    pointer_type,
                    true,
                )],
            ));
        }

        #[test]
        fn compile_pass_through() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![PassThrough::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(42),
                        "x",
                    )
                    .into()],
                    Return::new(types::Primitive::PointerInteger, Variable::new("x")),
                ),
                types::Primitive::PointerInteger,
                true,
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
                        "y",
                    )
                    .into()],
                    Return::new(types::Primitive::PointerInteger, Variable::new("y")),
                ),
                types::Primitive::PointerInteger,
                true,
            ));
        }

        #[test]
        fn compile_free_heap() {
            compile_function_definition(create_function_definition(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                Block::new(
                    vec![
                        FreeHeap::new(types::Primitive::PointerInteger, Variable::new("x")).into(),
                    ],
                    Return::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(0),
                    ),
                ),
                types::Primitive::PointerInteger,
                true,
            ));
        }
    }
}
