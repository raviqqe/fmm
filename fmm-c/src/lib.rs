mod expressions;
mod instructions;
mod malloc_configuration;
mod names;
mod renaming;
mod types;

use expressions::*;
use fmm::{analysis::collect_types, ir::*};
use instructions::*;
pub use malloc_configuration::MallocConfiguration;
use names::*;
use renaming::rename_names;
use std::collections::{HashMap, HashSet};
use types::*;

const INCLUDES: &[&str] = &[
    "#include <stdalign.h>",
    "#include <stdatomic.h>",
    "#include <stdbool.h>",
    "#include <stdint.h>",
    "#include <stdlib.h>",
];

pub fn compile(module: &Module, malloc_configuration: Option<MallocConfiguration>) -> String {
    let module = rename_names(module);
    let global_variables = module
        .variable_declarations()
        .iter()
        .map(|declaration| declaration.name().into())
        .chain(
            module
                .variable_definitions()
                .iter()
                .map(|declaration| declaration.name().into()),
        )
        .collect();
    let types = collect_types(&module);
    let type_ids = compile_type_ids(&types);

    INCLUDES
        .iter()
        .map(|&string| string.into())
        .chain(malloc_configuration.iter().flat_map(|configuration| {
            vec![
                format!(
                    "#define malloc(size) {}(size)",
                    &configuration.malloc_function_name
                ),
                format!("void* {}(size_t);", &configuration.malloc_function_name),
                format!(
                    "#define realloc(pointer,size) {}(pointer,size)",
                    &configuration.realloc_function_name
                ),
                format!(
                    "void* {}(void*,size_t);",
                    &configuration.realloc_function_name
                ),
            ]
        }))
        .chain(
            collect_types(&module)
                .iter()
                .filter_map(|type_| match type_ {
                    fmm::types::Type::Record(record) => {
                        Some(compile_record_type_definition(record, &type_ids))
                    }
                    fmm::types::Type::Union(union) => {
                        Some(compile_union_type_definition(union, &type_ids))
                    }
                    _ => None,
                }),
        )
        .chain(
            module
                .variable_declarations()
                .iter()
                .map(|declaration| compile_variable_declaration(declaration, &type_ids)),
        )
        .chain(
            module
                .variable_definitions()
                .iter()
                .map(|definition| compile_variable_forward_declaration(definition, &type_ids)),
        )
        .chain(
            module
                .function_declarations()
                .iter()
                .map(|declaration| compile_function_declaration(declaration, &type_ids)),
        )
        .chain(
            module
                .function_definitions()
                .iter()
                .map(|definition| compile_function_forward_declaration(definition, &type_ids)),
        )
        .chain(module.variable_definitions().iter().map(|definition| {
            compile_variable_definition(definition, &global_variables, &type_ids)
        }))
        .chain(module.function_definitions().iter().map(|definition| {
            compile_function_definition(definition, &global_variables, &type_ids)
        }))
        .collect::<Vec<_>>()
        .iter()
        .map(|string| string.as_str())
        .collect::<Vec<_>>()
        .join("\n")
}

fn compile_record_type_definition(
    record: &fmm::types::Record,
    type_ids: &HashMap<fmm::types::Type, String>,
) -> String {
    format!(
        "struct {} {{{}}};",
        type_ids[&record.clone().into()],
        compile_record_elements(record, type_ids)
    )
}

fn compile_union_type_definition(
    union: &fmm::types::Union,
    type_ids: &HashMap<fmm::types::Type, String>,
) -> String {
    format!(
        "union {} {{{}}};",
        type_ids[&union.clone().into()],
        compile_union_members(union, type_ids)
    )
}

fn compile_variable_declaration(
    declaration: &VariableDeclaration,
    type_ids: &HashMap<fmm::types::Type, String>,
) -> String {
    "extern ".to_owned()
        + &compile_typed_name(declaration.type_(), declaration.name(), type_ids)
        + ";"
}

fn compile_variable_forward_declaration(
    definition: &VariableDefinition,
    type_ids: &HashMap<fmm::types::Type, String>,
) -> String {
    compile_variable_definition_lhs(definition, type_ids) + ";"
}

fn compile_function_declaration(
    declaration: &FunctionDeclaration,
    type_ids: &HashMap<fmm::types::Type, String>,
) -> String {
    "extern ".to_owned()
        + &compile_function_name(declaration.type_(), declaration.name(), type_ids)
        + ";"
}

fn compile_function_forward_declaration(
    definition: &FunctionDefinition,
    type_ids: &HashMap<fmm::types::Type, String>,
) -> String {
    compile_linkage(definition.linkage()).to_owned()
        + &compile_function_name(definition.type_(), definition.name(), type_ids)
        + ";"
}

fn compile_variable_definition(
    definition: &VariableDefinition,
    global_variables: &HashSet<String>,
    type_ids: &HashMap<fmm::types::Type, String>,
) -> String {
    compile_variable_definition_lhs(definition, type_ids)
        + "="
        + &compile_expression(definition.body(), global_variables, type_ids)
        + ";"
}

fn compile_variable_definition_lhs(
    definition: &VariableDefinition,
    type_ids: &HashMap<fmm::types::Type, String>,
) -> String {
    compile_linkage(definition.linkage()).to_owned()
        + &compile_typed_name(
            definition.type_(),
            &(if definition.is_mutable() {
                ""
            } else {
                "const "
            }
            .to_owned()
                + definition.name()),
            type_ids,
        )
}

fn compile_function_definition(
    definition: &FunctionDefinition,
    global_variables: &HashSet<String>,
    type_ids: &HashMap<fmm::types::Type, String>,
) -> String {
    compile_linkage(definition.linkage()).to_owned()
        + &compile_typed_name(
            definition.result_type(),
            &format!(
                "{}({})",
                definition.name(),
                definition
                    .arguments()
                    .iter()
                    .map(|argument| compile_typed_name(argument.type_(), argument.name(), type_ids))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            type_ids,
        )
        + "{\n"
        + &compile_block(definition.body(), None, global_variables, type_ids)
        + "\n}"
}

fn compile_type_ids(types: &[fmm::types::Type]) -> HashMap<fmm::types::Type, String> {
    types
        .iter()
        .filter_map(|type_| {
            if let fmm::types::Type::Record(record) = type_ {
                Some(record)
            } else {
                None
            }
        })
        .collect::<HashSet<_>>()
        .into_iter()
        .enumerate()
        .map(|(index, record)| (record.clone().into(), generate_record_type_name(index)))
        .chain(
            types
                .iter()
                .filter_map(|type_| {
                    if let fmm::types::Type::Union(union) = type_ {
                        Some(union)
                    } else {
                        None
                    }
                })
                .collect::<HashSet<_>>()
                .into_iter()
                .enumerate()
                .map(|(index, union)| (union.clone().into(), generate_union_type_name(index))),
        )
        .collect()
}

fn compile_linkage(linkage: Linkage) -> &'static str {
    match linkage {
        fmm::ir::Linkage::External => "",
        fmm::ir::Linkage::Internal => "static ",
        fmm::ir::Linkage::Weak => unimplemented!("weak linkage not supported"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fmm::types::{self, CallingConvention, Type};

    fn compile_final_module(module: &Module) {
        fmm::analysis::check_types(module).unwrap();

        let directory = tempfile::tempdir().unwrap();
        let file_path = directory.path().join("foo.c");
        let source = compile(
            module,
            Some(MallocConfiguration {
                malloc_function_name: "my_malloc".into(),
                realloc_function_name: "my_realloc".into(),
            }),
        );

        println!("{}", source);

        std::fs::write(&file_path, source).unwrap();
        let output = std::process::Command::new("clang")
            .arg("-Werror") // cspell:disable-line
            .arg("-Wno-incompatible-pointer-types-discards-qualifiers") // cspell:disable-line
            .arg("-o")
            .arg(directory.path().join("foo.o"))
            .arg("-c")
            .arg(&file_path)
            .output()
            .unwrap();

        assert_eq!(String::from_utf8_lossy(&output.stdout), "");
        assert_eq!(String::from_utf8_lossy(&output.stderr), "");
        assert!(output.status.success());
    }

    fn compile_module(module: &Module) {
        compile_final_module(module);
        compile_final_module(
            &fmm::analysis::transform_to_cps(module, types::VOID_TYPE.clone()).unwrap(),
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
    fn rename_names_first() {
        compile_module(&Module::new(
            vec![VariableDeclaration::new(
                "😀",
                types::Primitive::PointerInteger,
            )],
            vec![],
            vec![],
            vec![],
        ));
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
                    Linkage::Internal,
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
        #[should_panic]
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
                    Linkage::Internal,
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
                    Linkage::Internal,
                )],
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
        #[should_panic]
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
                    Linkage::Internal,
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
                        Undefined::new(types::Pointer::new(record_type.clone())),
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
                        Undefined::new(types::Pointer::new(union_type.clone())),
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
        fn compile_allocate_heap() {
            compile_function_definition(create_function_definition(
                "f",
                vec![],
                Block::new(
                    vec![AllocateHeap::new(Primitive::PointerInteger(42), "y").into()],
                    Return::new(types::GENERIC_POINTER_TYPE.clone(), Variable::new("y")),
                ),
                types::GENERIC_POINTER_TYPE.clone(),
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
                    Return::new(types::GENERIC_POINTER_TYPE.clone(), Variable::new("y")),
                ),
                types::GENERIC_POINTER_TYPE.clone(),
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
                vec![Argument::new("x", types::GENERIC_POINTER_TYPE.clone())],
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
