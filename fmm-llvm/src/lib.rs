mod error;
mod expressions;
mod heap;
mod instructions;
mod types;

use error::CompileError;
use expressions::*;
use fmm::ir::*;
pub use heap::HeapConfiguration;
use instructions::*;
use types::*;

pub fn compile(
    module: &Module,
    target_triple: &str,
    heap_configuration: HeapConfiguration,
) -> Result<Vec<u8>, CompileError> {
    let context = inkwell::context::Context::create();

    let target_triple = inkwell::targets::TargetTriple::create(target_triple);
    let target_data = inkwell::targets::Target::from_triple(&target_triple)?
        .create_target_machine(
            &target_triple,
            "",
            "",
            Default::default(),
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .ok_or_else(|| CompileError::TargetMachineNotCreated)?
        .get_target_data();

    let llvm_module = context.create_module("");

    for declaration in module.variable_declarations() {
        compile_variable_declaration(&llvm_module, declaration, &context, &target_data);
    }

    for declaration in module.function_declarations() {
        compile_function_declaration(&llvm_module, declaration, &context, &target_data);
    }

    for definition in module.variable_definitions() {
        compile_variable_definition(&llvm_module, definition, &context, &target_data);
    }

    for definition in module.function_definitions() {
        compile_function_definition(&llvm_module, definition, &context, &target_data);
    }

    Ok(llvm_module.write_bitcode_to_memory().as_slice().to_vec())
}

fn compile_variable_declaration<'c>(
    module: &inkwell::module::Module<'c>,
    declaration: &VariableDeclaration,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) {
    module.add_global(
        compile_type(declaration.type_(), context, target_data),
        None,
        declaration.name(),
    );
}

fn compile_function_declaration<'c>(
    module: &inkwell::module::Module<'c>,
    declaration: &FunctionDeclaration,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) {
    module.add_function(
        declaration.name(),
        compile_function_type(declaration.type_(), context, target_data),
        None,
    );
}

fn compile_variable_definition<'c>(
    module: &inkwell::module::Module<'c>,
    definition: &VariableDefinition,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) {
    todo!()
}

fn compile_function_definition<'c>(
    module: &inkwell::module::Module<'c>,
    definition: &FunctionDefinition,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
) {
    module.add_function(
        definition.name(),
        compile_function_type(definition.type_(), context, target_data),
        Some(compile_linkage(definition.is_global())),
    );

    todo!()
}

fn compile_linkage(is_global: bool) -> inkwell::module::Linkage {
    if is_global {
        inkwell::module::Linkage::External
    } else {
        inkwell::module::Linkage::Private
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use fmm::types::{self, CallingConvention, Type};

//     fn compile_final_module(module: &Module) {
//         let directory = tempfile::tempdir().unwrap();
//         let file_path = directory.path().join("foo.c");
//         let source = compile(
//             module,
//             Some(HeapConfiguration {
//                 heap_function_name: "my_heap".into(),
//                 realloc_function_name: "my_realloc".into(),
//             }),
//         );

//         println!("{}", source);

//         std::fs::write(&file_path, source).unwrap();
//         let output = std::process::Command::new("clang")
//             .arg("-Werror") // cspell:disable-line
//             .arg("-Wno-incompatible-pointer-types-discards-qualifiers") // cspell:disable-line
//             .arg("-o")
//             .arg(directory.path().join("foo.o"))
//             .arg("-c")
//             .arg(&file_path)
//             .output()
//             .unwrap();

//         assert_eq!(String::from_utf8_lossy(&output.stdout), "");
//         assert_eq!(String::from_utf8_lossy(&output.stderr), "");
//         assert!(output.status.success());
//     }

//     fn compile_module(module: &Module) {
//         compile_final_module(module);
//         compile_final_module(
//             &fmm::analysis::transform_to_cps(module, types::Record::new(vec![])).unwrap(),
//         );
//     }

//     fn create_function_type(arguments: Vec<Type>, result: impl Into<Type>) -> types::Function {
//         types::Function::new(arguments, result, CallingConvention::Source)
//     }

//     fn create_function_definition(
//         name: impl Into<String>,
//         arguments: Vec<Argument>,
//         body: Block,
//         result_type: impl Into<Type>,
//         global: bool,
//     ) -> FunctionDefinition {
//         FunctionDefinition::new(
//             name,
//             arguments,
//             body,
//             result_type,
//             CallingConvention::Source,
//             global,
//         )
//     }

//     #[test]
//     fn compile_empty_module() {
//         compile_module(&Module::new(vec![], vec![], vec![], vec![]));
//     }

//     #[test]
//     fn rename_names_first() {
//         compile_module(&Module::new(
//             vec![VariableDeclaration::new(
//                 "😀",
//                 types::Primitive::PointerInteger,
//             )],
//             vec![],
//             vec![],
//             vec![],
//         ));
//     }

//     mod variable_declarations {
//         use super::*;

//         #[test]
//         fn compile_pointer_integer() {
//             compile_module(&Module::new(
//                 vec![VariableDeclaration::new(
//                     "x",
//                     types::Primitive::PointerInteger,
//                 )],
//                 vec![],
//                 vec![],
//                 vec![],
//             ));
//         }

//         #[test]
//         fn compile_pointer_integer_pointer() {
//             compile_module(&Module::new(
//                 vec![VariableDeclaration::new(
//                     "x",
//                     types::Pointer::new(types::Primitive::PointerInteger),
//                 )],
//                 vec![],
//                 vec![],
//                 vec![],
//             ));
//         }

//         #[test]
//         fn compile_function_pointer() {
//             compile_module(&Module::new(
//                 vec![VariableDeclaration::new(
//                     "x",
//                     create_function_type(vec![], types::Primitive::PointerInteger),
//                 )],
//                 vec![],
//                 vec![],
//                 vec![],
//             ));
//         }

//         #[test]
//         fn compile_reference_to_declared_variable() {
//             compile_module(&Module::new(
//                 vec![VariableDeclaration::new(
//                     "x",
//                     types::Primitive::PointerInteger,
//                 )],
//                 vec![],
//                 vec![],
//                 vec![create_function_definition(
//                     "f",
//                     vec![],
//                     Block::new(
//                         vec![],
//                         Return::new(
//                             types::Pointer::new(types::Primitive::PointerInteger),
//                             Variable::new("x"),
//                         ),
//                     ),
//                     types::Pointer::new(types::Primitive::PointerInteger),
//                     false,
//                 )],
//             ));
//         }
//     }

//     mod function_declarations {
//         use super::*;

//         #[test]
//         fn compile_function_pointer() {
//             compile_module(&Module::new(
//                 vec![],
//                 vec![FunctionDeclaration::new(
//                     "x",
//                     create_function_type(vec![], types::Primitive::PointerInteger),
//                 )],
//                 vec![],
//                 vec![],
//             ));
//         }
//     }

//     mod type_definitions {
//         use super::*;

//         #[test]
//         fn compile_record_type_definition() {
//             compile_module(&Module::new(
//                 vec![VariableDeclaration::new(
//                     "x",
//                     types::Record::new(vec![types::Primitive::PointerInteger.into()]),
//                 )],
//                 vec![],
//                 vec![],
//                 vec![],
//             ));
//         }

//         #[test]
//         fn compile_nested_record_type_definition() {
//             compile_module(&Module::new(
//                 vec![VariableDeclaration::new(
//                     "x",
//                     types::Record::new(vec![types::Record::new(vec![]).into()]),
//                 )],
//                 vec![],
//                 vec![],
//                 vec![],
//             ));
//         }

//         #[test]
//         fn compile_record_type_definition_with_nested_union_type() {
//             compile_module(&Module::new(
//                 vec![VariableDeclaration::new(
//                     "x",
//                     types::Record::new(vec![types::Union::new(vec![
//                         types::Primitive::PointerInteger.into(),
//                     ])
//                     .into()]),
//                 )],
//                 vec![],
//                 vec![],
//                 vec![],
//             ));
//         }

//         #[test]
//         fn compile_union_type_definition() {
//             compile_module(&Module::new(
//                 vec![VariableDeclaration::new(
//                     "x",
//                     types::Union::new(vec![types::Primitive::PointerInteger.into()]),
//                 )],
//                 vec![],
//                 vec![],
//                 vec![],
//             ));
//         }

//         #[test]
//         fn compile_nested_union_type_definition() {
//             compile_module(&Module::new(
//                 vec![VariableDeclaration::new(
//                     "x",
//                     types::Union::new(vec![types::Union::new(vec![
//                         types::Primitive::PointerInteger.into(),
//                     ])
//                     .into()]),
//                 )],
//                 vec![],
//                 vec![],
//                 vec![],
//             ));
//         }

//         #[test]
//         fn compile_union_type_definition_with_nested_record_type() {
//             compile_module(&Module::new(
//                 vec![VariableDeclaration::new(
//                     "x",
//                     types::Union::new(vec![types::Record::new(vec![]).into()]),
//                 )],
//                 vec![],
//                 vec![],
//                 vec![],
//             ));
//         }
//     }

//     mod variable_definitions {
//         use super::*;

//         #[test]
//         fn compile_constant_variable() {
//             compile_module(&Module::new(
//                 vec![],
//                 vec![],
//                 vec![VariableDefinition::new(
//                     "x",
//                     fmm::ir::Primitive::PointerInteger(0),
//                     types::Primitive::PointerInteger,
//                     false,
//                     true,
//                 )],
//                 vec![],
//             ));
//         }

//         #[test]
//         fn compile_mutable_variable() {
//             compile_module(&Module::new(
//                 vec![],
//                 vec![],
//                 vec![VariableDefinition::new(
//                     "x",
//                     fmm::ir::Primitive::PointerInteger(0),
//                     types::Primitive::PointerInteger,
//                     true,
//                     true,
//                 )],
//                 vec![],
//             ));
//         }

//         #[test]
//         fn compile_local_variable() {
//             compile_module(&Module::new(
//                 vec![],
//                 vec![],
//                 vec![VariableDefinition::new(
//                     "x",
//                     fmm::ir::Primitive::PointerInteger(0),
//                     types::Primitive::PointerInteger,
//                     true,
//                     false,
//                 )],
//                 vec![],
//             ));
//         }

//         #[test]
//         fn compile_reference_to_defined_variable() {
//             compile_module(&Module::new(
//                 vec![],
//                 vec![],
//                 vec![VariableDefinition::new(
//                     "x",
//                     fmm::ir::Primitive::PointerInteger(0),
//                     types::Primitive::PointerInteger,
//                     false,
//                     false,
//                 )],
//                 vec![create_function_definition(
//                     "f",
//                     vec![],
//                     Block::new(
//                         vec![],
//                         Return::new(
//                             types::Pointer::new(types::Primitive::PointerInteger),
//                             Variable::new("x"),
//                         ),
//                     ),
//                     types::Pointer::new(types::Primitive::PointerInteger),
//                     false,
//                 )],
//             ));
//         }
//     }

//     mod function_definitions {
//         use super::*;

//         #[test]
//         fn compile_global_function() {
//             compile_module(&Module::new(
//                 vec![],
//                 vec![],
//                 vec![],
//                 vec![create_function_definition(
//                     "x",
//                     vec![],
//                     fmm::ir::Block::new(
//                         vec![],
//                         fmm::ir::Return::new(
//                             types::Primitive::PointerInteger,
//                             fmm::ir::Primitive::PointerInteger(0),
//                         ),
//                     ),
//                     types::Primitive::PointerInteger,
//                     true,
//                 )],
//             ));
//         }

//         #[test]
//         fn compile_local_function() {
//             compile_module(&Module::new(
//                 vec![],
//                 vec![],
//                 vec![],
//                 vec![create_function_definition(
//                     "x",
//                     vec![],
//                     fmm::ir::Block::new(
//                         vec![],
//                         fmm::ir::Return::new(
//                             types::Primitive::PointerInteger,
//                             fmm::ir::Primitive::PointerInteger(0),
//                         ),
//                     ),
//                     types::Primitive::PointerInteger,
//                     false,
//                 )],
//             ));
//         }
//     }

//     mod expressions {
//         use super::*;

//         #[test]
//         fn compile_size_of() {
//             compile_module(&Module::new(
//                 vec![],
//                 vec![],
//                 vec![VariableDefinition::new(
//                     "x",
//                     fmm::ir::SizeOf::new(types::Primitive::Float64),
//                     types::Primitive::PointerInteger,
//                     false,
//                     false,
//                 )],
//                 vec![],
//             ));
//         }

//         #[test]
//         fn compile_align_of() {
//             compile_module(&Module::new(
//                 vec![],
//                 vec![],
//                 vec![VariableDefinition::new(
//                     "x",
//                     fmm::ir::AlignOf::new(types::Primitive::Float64),
//                     types::Primitive::PointerInteger,
//                     false,
//                     false,
//                 )],
//                 vec![],
//             ));
//         }

//         #[test]
//         fn compile_bit_cast() {
//             compile_module(&Module::new(
//                 vec![],
//                 vec![],
//                 vec![VariableDefinition::new(
//                     "x",
//                     BitCast::new(
//                         types::Primitive::Integer64,
//                         types::Primitive::Float64,
//                         Primitive::Integer64(42),
//                     ),
//                     types::Primitive::Float64,
//                     false,
//                     true,
//                 )],
//                 vec![],
//             ));
//         }
//     }

//     mod instructions {
//         use super::*;

//         fn compile_function_definition(definition: FunctionDefinition) {
//             compile_module(&Module::new(vec![], vec![], vec![], vec![definition]));
//         }

//         #[test]
//         fn compile_unreachable() {
//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![],
//                 Block::new(vec![], TerminalInstruction::Unreachable),
//                 types::Primitive::PointerInteger,
//                 true,
//             ));
//         }

//         #[test]
//         fn compile_allocate_heap() {
//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![],
//                 Block::new(
//                     vec![AllocateHeap::new(types::Primitive::PointerInteger, "y").into()],
//                     Return::new(
//                         types::Pointer::new(types::Primitive::PointerInteger),
//                         Variable::new("y"),
//                     ),
//                 ),
//                 types::Pointer::new(types::Primitive::PointerInteger),
//                 true,
//             ));
//         }

//         #[test]
//         fn compile_reallocate_heap() {
//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![],
//                 Block::new(
//                     vec![
//                         AllocateHeap::new(types::Primitive::Integer8, "x").into(),
//                         ReallocateHeap::new(Variable::new("x"), Primitive::PointerInteger(42), "y")
//                             .into(),
//                     ],
//                     Return::new(
//                         types::Pointer::new(types::Primitive::Integer8),
//                         Variable::new("y"),
//                     ),
//                 ),
//                 types::Pointer::new(types::Primitive::Integer8),
//                 true,
//             ));
//         }

//         #[test]
//         fn compile_allocate_stack() {
//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![],
//                 Block::new(
//                     vec![AllocateStack::new(types::Primitive::PointerInteger, "y").into()],
//                     Return::new(
//                         types::Pointer::new(types::Primitive::PointerInteger),
//                         Variable::new("y"),
//                     ),
//                 ),
//                 types::Pointer::new(types::Primitive::PointerInteger),
//                 true,
//             ));
//         }

//         #[test]
//         fn compile_allocate_heap_with_function_pointer() {
//             let function_type = create_function_type(
//                 vec![types::Primitive::PointerInteger.into()],
//                 types::Primitive::PointerInteger,
//             );

//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![],
//                 Block::new(
//                     vec![AllocateHeap::new(function_type.clone(), "y").into()],
//                     Return::new(
//                         types::Pointer::new(function_type.clone()),
//                         Variable::new("y"),
//                     ),
//                 ),
//                 types::Pointer::new(function_type),
//                 true,
//             ));
//         }

//         #[test]
//         fn compile_arithmetic_operation() {
//             for &operator in &[
//                 ArithmeticOperator::Add,
//                 ArithmeticOperator::Subtract,
//                 ArithmeticOperator::Multiply,
//                 ArithmeticOperator::Divide,
//             ] {
//                 compile_function_definition(create_function_definition(
//                     "f",
//                     vec![],
//                     Block::new(
//                         vec![ArithmeticOperation::new(
//                             types::Primitive::PointerInteger,
//                             operator,
//                             Primitive::PointerInteger(1),
//                             Primitive::PointerInteger(1),
//                             "x",
//                         )
//                         .into()],
//                         Return::new(types::Primitive::PointerInteger, Variable::new("x")),
//                     ),
//                     types::Primitive::PointerInteger,
//                     true,
//                 ));
//             }
//         }

//         #[test]
//         fn compile_comparison_operation() {
//             for &operator in &[
//                 ComparisonOperator::Equal,
//                 ComparisonOperator::NotEqual,
//                 ComparisonOperator::LessThan,
//                 ComparisonOperator::GreaterThan,
//                 ComparisonOperator::LessThanOrEqual,
//                 ComparisonOperator::GreaterThanOrEqual,
//             ] {
//                 compile_function_definition(create_function_definition(
//                     "f",
//                     vec![],
//                     Block::new(
//                         vec![ComparisonOperation::new(
//                             types::Primitive::PointerInteger,
//                             operator,
//                             Primitive::PointerInteger(1),
//                             Primitive::PointerInteger(1),
//                             "x",
//                         )
//                         .into()],
//                         Return::new(types::Primitive::Boolean, Variable::new("x")),
//                     ),
//                     types::Primitive::Boolean,
//                     true,
//                 ));
//             }
//         }

//         #[test]
//         fn compile_atomic_load() {
//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![Argument::new(
//                     "x",
//                     types::Pointer::new(types::Primitive::PointerInteger),
//                 )],
//                 Block::new(
//                     vec![AtomicLoad::new(
//                         types::Primitive::PointerInteger,
//                         Variable::new("x"),
//                         "y",
//                     )
//                     .into()],
//                     Return::new(types::Primitive::PointerInteger, Variable::new("y")),
//                 ),
//                 types::Primitive::PointerInteger,
//                 true,
//             ));
//         }

//         #[test]
//         fn compile_atomic_load_with_function_pointer() {
//             let function_type = create_function_type(
//                 vec![types::Primitive::PointerInteger.into()],
//                 types::Primitive::PointerInteger,
//             );

//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![Argument::new(
//                     "x",
//                     types::Pointer::new(function_type.clone()),
//                 )],
//                 Block::new(
//                     vec![AtomicLoad::new(function_type.clone(), Variable::new("x"), "y").into()],
//                     Return::new(function_type.clone(), Variable::new("y")),
//                 ),
//                 function_type,
//                 true,
//             ));
//         }

//         #[test]
//         fn compile_atomic_store() {
//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![Argument::new(
//                     "x",
//                     types::Pointer::new(types::Primitive::PointerInteger),
//                 )],
//                 Block::new(
//                     vec![AtomicStore::new(
//                         types::Primitive::PointerInteger,
//                         Undefined::new(types::Primitive::PointerInteger),
//                         Variable::new("x"),
//                     )
//                     .into()],
//                     Return::new(
//                         types::Primitive::PointerInteger,
//                         Primitive::PointerInteger(42),
//                     ),
//                 ),
//                 types::Primitive::PointerInteger,
//                 true,
//             ));
//         }

//         #[test]
//         fn compile_atomic_store_with_function_pointer() {
//             let function_type = create_function_type(
//                 vec![types::Primitive::PointerInteger.into()],
//                 types::Primitive::PointerInteger,
//             );

//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![Argument::new(
//                     "x",
//                     types::Pointer::new(function_type.clone()),
//                 )],
//                 Block::new(
//                     vec![AtomicStore::new(
//                         function_type.clone(),
//                         Undefined::new(function_type),
//                         Variable::new("x"),
//                     )
//                     .into()],
//                     Return::new(
//                         types::Primitive::PointerInteger,
//                         Primitive::PointerInteger(42),
//                     ),
//                 ),
//                 types::Primitive::PointerInteger,
//                 true,
//             ));
//         }

//         #[test]
//         fn compile_if() {
//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![],
//                 Block::new(
//                     vec![If::new(
//                         types::Primitive::PointerInteger,
//                         Primitive::Boolean(true),
//                         Block::new(
//                             vec![],
//                             Branch::new(
//                                 types::Primitive::PointerInteger,
//                                 Primitive::PointerInteger(42),
//                             ),
//                         ),
//                         Block::new(
//                             vec![],
//                             Branch::new(
//                                 types::Primitive::PointerInteger,
//                                 Primitive::PointerInteger(42),
//                             ),
//                         ),
//                         "x",
//                     )
//                     .into()],
//                     Return::new(types::Primitive::PointerInteger, Variable::new("x")),
//                 ),
//                 types::Primitive::PointerInteger,
//                 true,
//             ));
//         }

//         #[test]
//         fn compile_if_with_return() {
//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![],
//                 Block::new(
//                     vec![If::new(
//                         types::Primitive::PointerInteger,
//                         Primitive::Boolean(true),
//                         Block::new(
//                             vec![],
//                             Return::new(
//                                 types::Primitive::PointerInteger,
//                                 Primitive::PointerInteger(42),
//                             ),
//                         ),
//                         Block::new(
//                             vec![],
//                             Branch::new(
//                                 types::Primitive::PointerInteger,
//                                 Primitive::PointerInteger(42),
//                             ),
//                         ),
//                         "x",
//                     )
//                     .into()],
//                     Return::new(types::Primitive::PointerInteger, Variable::new("x")),
//                 ),
//                 types::Primitive::PointerInteger,
//                 true,
//             ));
//         }

//         #[test]
//         fn compile_if_with_unreachable() {
//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![],
//                 Block::new(
//                     vec![If::new(
//                         types::Primitive::PointerInteger,
//                         Primitive::Boolean(true),
//                         Block::new(vec![], TerminalInstruction::Unreachable),
//                         Block::new(
//                             vec![],
//                             Branch::new(
//                                 types::Primitive::PointerInteger,
//                                 Primitive::PointerInteger(42),
//                             ),
//                         ),
//                         "x",
//                     )
//                     .into()],
//                     Return::new(types::Primitive::PointerInteger, Variable::new("x")),
//                 ),
//                 types::Primitive::PointerInteger,
//                 true,
//             ));
//         }

//         #[test]
//         fn compile_deconstruct_record() {
//             let record_type = types::Record::new(vec![types::Primitive::PointerInteger.into()]);

//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![],
//                 Block::new(
//                     vec![DeconstructRecord::new(
//                         record_type.clone(),
//                         Record::new(record_type, vec![Primitive::PointerInteger(42).into()]),
//                         0,
//                         "x",
//                     )
//                     .into()],
//                     Return::new(types::Primitive::PointerInteger, Variable::new("x")),
//                 ),
//                 types::Primitive::PointerInteger,
//                 true,
//             ));
//         }

//         #[test]
//         fn compile_deconstruct_union() {
//             let union_type = types::Union::new(vec![types::Primitive::PointerInteger.into()]);

//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![],
//                 Block::new(
//                     vec![DeconstructUnion::new(
//                         union_type.clone(),
//                         Union::new(union_type, 0, Primitive::PointerInteger(42)),
//                         0,
//                         "x",
//                     )
//                     .into()],
//                     Return::new(types::Primitive::PointerInteger, Variable::new("x")),
//                 ),
//                 types::Primitive::PointerInteger,
//                 true,
//             ));
//         }

//         #[test]
//         fn compile_compare_and_swap() {
//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![Argument::new(
//                     "x",
//                     types::Pointer::new(types::Primitive::PointerInteger),
//                 )],
//                 Block::new(
//                     vec![CompareAndSwap::new(
//                         types::Primitive::PointerInteger,
//                         Variable::new("x"),
//                         Primitive::PointerInteger(0),
//                         Primitive::PointerInteger(1),
//                         "y",
//                     )
//                     .into()],
//                     Return::new(types::Primitive::Boolean, Variable::new("y")),
//                 ),
//                 types::Primitive::Boolean,
//                 true,
//             ));
//         }

//         #[test]
//         fn compile_record_address() {
//             let record_type = types::Record::new(vec![types::Primitive::PointerInteger.into()]);
//             let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![Argument::new("x", types::Pointer::new(record_type.clone()))],
//                 Block::new(
//                     vec![RecordAddress::new(record_type, Variable::new("x"), 0, "y").into()],
//                     Return::new(pointer_type.clone(), Variable::new("y")),
//                 ),
//                 pointer_type,
//                 true,
//             ));
//         }

//         #[test]
//         fn compile_record_address_with_global_variable() {
//             let record_type = types::Record::new(vec![types::Primitive::PointerInteger.into()]);
//             let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

//             compile_module(&Module::new(
//                 vec![VariableDeclaration::new("x", record_type.clone())],
//                 vec![],
//                 vec![],
//                 vec![create_function_definition(
//                     "f",
//                     vec![],
//                     Block::new(
//                         vec![RecordAddress::new(record_type, Variable::new("x"), 0, "y").into()],
//                         Return::new(pointer_type.clone(), Variable::new("y")),
//                     ),
//                     pointer_type,
//                     true,
//                 )],
//             ));
//         }

//         #[test]
//         fn compile_union_address() {
//             let union_type = types::Union::new(vec![
//                 types::Primitive::PointerInteger.into(),
//                 types::Primitive::Float64.into(),
//             ]);
//             let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![Argument::new("x", types::Pointer::new(union_type.clone()))],
//                 Block::new(
//                     vec![UnionAddress::new(union_type, Variable::new("x"), 0, "y").into()],
//                     Return::new(pointer_type.clone(), Variable::new("y")),
//                 ),
//                 pointer_type,
//                 true,
//             ));
//         }

//         #[test]
//         fn compile_union_address_with_global_variable() {
//             let union_type = types::Union::new(vec![
//                 types::Primitive::PointerInteger.into(),
//                 types::Primitive::Float64.into(),
//             ]);
//             let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

//             compile_module(&Module::new(
//                 vec![VariableDeclaration::new("x", union_type.clone())],
//                 vec![],
//                 vec![],
//                 vec![create_function_definition(
//                     "f",
//                     vec![],
//                     Block::new(
//                         vec![UnionAddress::new(union_type, Variable::new("x"), 0, "y").into()],
//                         Return::new(pointer_type.clone(), Variable::new("y")),
//                     ),
//                     pointer_type,
//                     true,
//                 )],
//             ));
//         }

//         #[test]
//         fn compile_pass_through() {
//             compile_function_definition(create_function_definition(
//                 "f",
//                 vec![],
//                 Block::new(
//                     vec![PassThrough::new(
//                         types::Primitive::PointerInteger,
//                         Primitive::PointerInteger(42),
//                         "x",
//                     )
//                     .into()],
//                     Return::new(types::Primitive::PointerInteger, Variable::new("x")),
//                 ),
//                 types::Primitive::PointerInteger,
//                 true,
//             ));
//         }
//     }
// }
