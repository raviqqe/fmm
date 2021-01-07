mod error;
mod expressions;
mod instructions;
mod names;
mod types;

pub use error::*;
use expressions::*;
use fmm::analysis::*;
use fmm::ir::*;
use indoc::indoc;
use instructions::*;
use names::*;

const INCLUDES: &str = indoc! {"
    #include <stdatomic.h>
    #include <stdbool.h>
    #include <stdint.h>
    #include <stdlib.h>
"};

pub fn compile(module: &Module) -> String {
    let types = collect_types(module);

    let strings = vec![INCLUDES.into()]
        .into_iter()
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
        types::compile_record_elements(record)
    )
}

fn compile_union_type_definition(union: &fmm::types::Union) -> String {
    format!(
        "union {} {{{}}};",
        generate_union_type_name(union),
        types::compile_union_members(union)
    )
}

fn compile_variable_declaration(declaration: &VariableDeclaration) -> String {
    "extern ".to_owned() + &types::compile_typed_name(declaration.type_(), declaration.name()) + ";"
}

fn compile_variable_forward_declaration(definition: &VariableDefinition) -> String {
    types::compile_typed_name(
        definition.type_(),
        &(if definition.is_mutable() { "" } else { "const" }.to_owned() + " " + definition.name()),
    ) + ";"
}

fn compile_function_declaration(declaration: &FunctionDeclaration) -> String {
    "extern ".to_owned()
        + &types::compile_function_name(declaration.type_(), declaration.name())
        + ";"
}

fn compile_function_forward_declaration(definition: &FunctionDefinition) -> String {
    types::compile_function_name(definition.type_(), definition.name()) + ";"
}

fn compile_variable_definition(definition: &VariableDefinition) -> String {
    types::compile_typed_name(
        definition.type_(),
        &(if definition.is_mutable() {
            ""
        } else {
            "const "
        }
        .to_owned()
            + definition.name()),
    ) + " = "
        + &compile_expression(definition.body())
        + ";"
}

fn compile_function_definition(definition: &FunctionDefinition) -> String {
    types::compile_function_name(definition.type_(), definition.name())
        + "{"
        + &compile_block(definition.body())
        + "}"
}

#[cfg(test)]
mod tests {
    use super::*;
    use fmm::types;

    #[test]
    fn compile_empty_module() {
        insta::assert_snapshot!(compile(&Module::new(vec![], vec![], vec![], vec![])));
    }

    mod variable_declarations {
        use super::*;

        #[test]
        fn compile_pointer_integer() {
            insta::assert_snapshot!(compile(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Primitive::PointerInteger
                )],
                vec![],
                vec![],
                vec![]
            )));
        }

        #[test]
        fn compile_pointer_integer_pointer() {
            insta::assert_snapshot!(compile(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger)
                )],
                vec![],
                vec![],
                vec![]
            )));
        }

        #[test]
        fn compile_function_pointer() {
            insta::assert_snapshot!(compile(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Function::new(vec![], types::Primitive::PointerInteger)
                )],
                vec![],
                vec![],
                vec![]
            )));
        }
    }

    mod function_declarations {
        use super::*;

        #[test]
        fn compile_function_pointer() {
            insta::assert_snapshot!(compile(&Module::new(
                vec![],
                vec![FunctionDeclaration::new(
                    "x",
                    types::Function::new(vec![], types::Primitive::PointerInteger)
                )],
                vec![],
                vec![]
            )));
        }
    }

    mod type_definitions {
        use super::*;

        #[test]
        fn compile_record_type_definition() {
            insta::assert_snapshot!(compile(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Record::new(vec![types::Primitive::PointerInteger.into()])
                )],
                vec![],
                vec![],
                vec![]
            )));
        }

        #[test]
        fn compile_union_type_definition() {
            insta::assert_snapshot!(compile(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Union::new(vec![types::Primitive::PointerInteger.into()])
                )],
                vec![],
                vec![],
                vec![]
            )));
        }
    }
}
