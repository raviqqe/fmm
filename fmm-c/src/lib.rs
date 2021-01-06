mod error;
mod names;
mod types;

pub use error::*;
use fmm::ir::*;
use indoc::indoc;
use types::*;

const INCLUDES: &str = indoc! {"
    #include <stdatomic.h>
    #include <stdbool.h>
    #include <stdint.h>
    #include <stdlib.h>
"};

pub fn compile(module: &Module) -> String {
    let strings = vec![INCLUDES.into()]
        .into_iter()
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
        .collect::<Vec<_>>();

    strings
        .iter()
        .map(|string| string.as_str())
        .collect::<Vec<_>>()
        .join("\n")
}

fn compile_variable_declaration(declaration: &VariableDeclaration) -> String {
    "extern ".to_owned() + &compile_typed_name(declaration.type_(), declaration.name()) + ";"
}

fn compile_variable_forward_declaration(definition: &VariableDefinition) -> String {
    compile_typed_name(
        definition.type_(),
        &(if definition.is_mutable() { "" } else { "const" }.to_owned() + " " + definition.name()),
    ) + ";"
}

fn compile_function_declaration(function_declaration: &FunctionDeclaration) -> String {
    "extern ".to_owned()
        + &compile_function_name(function_declaration.type_(), function_declaration.name())
        + ";"
}

fn compile_function_forward_declaration(definition: &FunctionDefinition) -> String {
    compile_function_name(definition.type_(), definition.name()) + ";"
}
