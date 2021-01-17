mod error;
mod expressions;
mod instructions;
mod names;
mod types;

pub use error::*;
use expressions::*;
use fmm::analysis::*;
use fmm::ir::*;
use instructions::*;
use names::*;
use types::*;

const INCLUDES: &[&str] = &[
    "#include <stdatomic.h>",
    "#include <stdbool.h>",
    "#include <stdint.h>",
    "#include <stdlib.h>",
];

pub fn compile(module: &Module) -> String {
    check_types(module).unwrap();

    let types = collect_types(module);

    let strings = INCLUDES
        .iter()
        .map(|&string| string.into())
        .chain(
            collect_record_types(&types)
                .iter()
                .map(compile_record_type_definition),
        )
        .chain(
            collect_union_types(&types)
                .iter()
                .map(compile_union_type_definition),
        )
        .chain(
            module
                .variable_declarations()
                .iter()
                .map(compile_variable_declaration),
        )
        .chain(
            module
                .variable_definitions()
                .iter()
                .map(compile_variable_forward_declaration),
        )
        .chain(
            module
                .function_declarations()
                .iter()
                .map(compile_function_declaration),
        )
        .chain(
            module
                .function_definitions()
                .iter()
                .map(compile_function_forward_declaration),
        )
        .chain(
            module
                .variable_definitions()
                .iter()
                .map(compile_variable_definition),
        )
        .chain(
            module
                .function_definitions()
                .iter()
                .map(compile_function_definition),
        )
        .collect::<Vec<_>>();

    strings
        .iter()
        .map(|string| string.as_str())
        .collect::<Vec<_>>()
        .join("\n")
}

fn compile_record_type_definition(record: &fmm::types::Record) -> String {
    format!(
        "struct {} {{{}}};",
        generate_record_type_name(record),
        compile_record_elements(record)
    )
}

fn compile_union_type_definition(union: &fmm::types::Union) -> String {
    format!(
        "union {} {{{}}};",
        generate_union_type_name(union),
        compile_union_members(union)
    )
}

fn compile_variable_declaration(declaration: &VariableDeclaration) -> String {
    "extern ".to_owned() + &compile_typed_name(declaration.type_(), declaration.name()) + ";"
}

fn compile_variable_forward_declaration(definition: &VariableDefinition) -> String {
    compile_variable_definition_lhs(definition) + ";"
}

fn compile_function_declaration(declaration: &FunctionDeclaration) -> String {
    "extern ".to_owned() + &compile_function_name(declaration.type_(), declaration.name()) + ";"
}

fn compile_function_forward_declaration(definition: &FunctionDefinition) -> String {
    if definition.is_global() {
        ""
    } else {
        "static "
    }
    .to_owned()
        + &compile_function_name(definition.type_(), definition.name())
        + ";"
}

fn compile_variable_definition(definition: &VariableDefinition) -> String {
    let entity_name = format!("{}_entity", definition.name());

    "static ".to_owned()
        + &compile_typed_name(
            definition.type_(),
            &(if definition.is_mutable() {
                ""
            } else {
                "const "
            }
            .to_owned()
                + &entity_name),
        )
        + " = "
        + &compile_expression(definition.body())
        + ";\n"
        + &compile_variable_definition_lhs(definition)
        + &format!(" = &{};", entity_name)
}

fn compile_variable_definition_lhs(definition: &VariableDefinition) -> String {
    if definition.is_global() {
        ""
    } else {
        "static "
    }
    .to_owned()
        + &compile_typed_name(
            definition.type_(),
            &(if definition.is_mutable() {
                ""
            } else {
                "const "
            }
            .to_owned()
                + "*const "
                + definition.name()),
        )
}

fn compile_function_definition(definition: &FunctionDefinition) -> String {
    if definition.is_global() {
        ""
    } else {
        "static "
    }
    .to_owned()
        + &compile_typed_name(
            definition.result_type(),
            &format!(
                "{}({})",
                definition.name(),
                definition
                    .arguments()
                    .iter()
                    .map(|argument| compile_typed_name(argument.type_(), argument.name()))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
        )
        + "{\n"
        + &compile_block(definition.body(), None)
        + "\n}"
}

#[cfg(test)]
mod tests {
    use super::*;
    use fmm::types;

    fn compile_module(module: &Module) {
        let directory = tempfile::tempdir().unwrap();
        let file_path = directory.path().join("foo.c");
        let source = compile(module);

        println!("{}", source);

        std::fs::write(&file_path, source).unwrap();
        let output = std::process::Command::new("clang")
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
                    types::Function::new(vec![], types::Primitive::PointerInteger),
                )],
                vec![],
                vec![],
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
                    types::Function::new(vec![], types::Primitive::PointerInteger),
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
    }

    mod function_definitions {
        use super::*;

        #[test]
        fn compile_global_function() {
            compile_module(&Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
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
                vec![FunctionDefinition::new(
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

    mod instructions {
        use super::*;

        fn compile_function_definition(definition: FunctionDefinition) {
            compile_module(&Module::new(vec![], vec![], vec![], vec![definition]));
        }

        #[test]
        fn compile_unreachable() {
            compile_function_definition(FunctionDefinition::new(
                "f",
                vec![],
                Block::new(vec![], TerminalInstruction::Unreachable),
                types::Primitive::PointerInteger,
                true,
            ));
        }

        #[test]
        fn compile_allocate_heap() {
            compile_function_definition(FunctionDefinition::new(
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
        fn compile_allocate_heap_with_function_pointer() {
            let function_type = types::Function::new(
                vec![types::Primitive::PointerInteger.into()],
                types::Primitive::PointerInteger,
            );

            compile_function_definition(FunctionDefinition::new(
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
        fn compile_arithmetic_operation() {
            for &operator in &[
                ArithmeticOperator::Add,
                ArithmeticOperator::Subtract,
                ArithmeticOperator::Multiply,
                ArithmeticOperator::Divide,
            ] {
                compile_function_definition(FunctionDefinition::new(
                    "f",
                    vec![],
                    Block::new(
                        vec![ArithmeticOperation::new(
                            types::Primitive::PointerInteger,
                            operator,
                            Primitive::PointerInteger(1),
                            Primitive::PointerInteger(1),
                            "x",
                        )
                        .into()],
                        Return::new(types::Primitive::PointerInteger, Variable::new("x")),
                    ),
                    types::Primitive::PointerInteger,
                    true,
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
                compile_function_definition(FunctionDefinition::new(
                    "f",
                    vec![],
                    Block::new(
                        vec![ComparisonOperation::new(
                            types::Primitive::PointerInteger,
                            operator,
                            Primitive::PointerInteger(1),
                            Primitive::PointerInteger(1),
                            "x",
                        )
                        .into()],
                        Return::new(types::Primitive::Bool, Variable::new("x")),
                    ),
                    types::Primitive::Bool,
                    true,
                ));
            }
        }

        #[test]
        fn compile_atomic_load() {
            compile_function_definition(FunctionDefinition::new(
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
            let function_type = types::Function::new(
                vec![types::Primitive::PointerInteger.into()],
                types::Primitive::PointerInteger,
            );

            compile_function_definition(FunctionDefinition::new(
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
            compile_function_definition(FunctionDefinition::new(
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
            let function_type = types::Function::new(
                vec![types::Primitive::PointerInteger.into()],
                types::Primitive::PointerInteger,
            );

            compile_function_definition(FunctionDefinition::new(
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
            compile_function_definition(FunctionDefinition::new(
                "f",
                vec![],
                Block::new(
                    vec![If::new(
                        types::Primitive::PointerInteger,
                        Primitive::Bool(true),
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
            compile_function_definition(FunctionDefinition::new(
                "f",
                vec![],
                Block::new(
                    vec![If::new(
                        types::Primitive::PointerInteger,
                        Primitive::Bool(true),
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
            compile_function_definition(FunctionDefinition::new(
                "f",
                vec![],
                Block::new(
                    vec![If::new(
                        types::Primitive::PointerInteger,
                        Primitive::Bool(true),
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
        fn compile_deconstruct_record() {
            let record_type = types::Record::new(vec![types::Primitive::PointerInteger.into()]);

            compile_function_definition(FunctionDefinition::new(
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

            compile_function_definition(FunctionDefinition::new(
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
            compile_function_definition(FunctionDefinition::new(
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
                    Return::new(types::Primitive::Bool, Variable::new("y")),
                ),
                types::Primitive::Bool,
                true,
            ));
        }
    }
}
