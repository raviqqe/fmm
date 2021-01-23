use crate::ir::*;
use std::collections::HashMap;

pub fn rename_globals(module: &Module, names: &HashMap<String, String>) -> Module {
    Module::new(
        module
            .variable_declarations()
            .iter()
            .map(|declaration| rename_variable_declaration(declaration, names))
            .collect(),
        module
            .function_declarations()
            .iter()
            .map(|declaration| rename_function_declaration(declaration, names))
            .collect(),
        module
            .variable_definitions()
            .iter()
            .map(|definition| rename_variable_definition(definition, names))
            .collect(),
        module
            .function_definitions()
            .iter()
            .map(|definition| rename_function_definition(definition, names))
            .collect(),
    )
}

fn rename_variable_declaration(
    declaration: &VariableDeclaration,
    names: &HashMap<String, String>,
) -> VariableDeclaration {
    VariableDeclaration::new(
        rename_name(declaration.name(), names),
        declaration.type_().clone(),
    )
}

fn rename_function_declaration(
    declaration: &FunctionDeclaration,
    names: &HashMap<String, String>,
) -> FunctionDeclaration {
    FunctionDeclaration::new(
        rename_name(declaration.name(), names),
        declaration.type_().clone(),
    )
}

fn rename_variable_definition(
    definition: &VariableDefinition,
    names: &HashMap<String, String>,
) -> VariableDefinition {
    VariableDefinition::new(
        rename_name(definition.name(), names),
        rename_expression(definition.body(), names),
        definition.type_().clone(),
        definition.is_mutable(),
        definition.is_global(),
    )
}

fn rename_function_definition(
    definition: &FunctionDefinition,
    names: &HashMap<String, String>,
) -> FunctionDefinition {
    FunctionDefinition::new(
        rename_name(definition.name(), names),
        definition.arguments().to_vec(),
        rename_block(definition.body(), names),
        definition.result_type().clone(),
        definition.is_global(),
    )
}

fn rename_block(block: &Block, names: &HashMap<String, String>) -> Block {
    let mut names = names.clone();
    let mut instructions = vec![];

    for instruction in block.instructions() {
        instructions.push(rename_instruction(instruction, &names));

        if let Some(name) = instruction.name() {
            names.remove(name);
        }
    }

    Block::new(
        instructions,
        rename_terminal_instruction(block.terminal_instruction(), &names),
    )
}

fn rename_instruction(instruction: &Instruction, names: &HashMap<String, String>) -> Instruction {
    let rename_expression = |expression| rename_expression(expression, names);

    match instruction {
        Instruction::ArithmeticOperation(operation) => ArithmeticOperation::new(
            operation.type_().clone(),
            operation.operator(),
            rename_expression(operation.lhs()),
            rename_expression(operation.rhs()),
            operation.name(),
        )
        .into(),
        Instruction::AtomicLoad(load) => AtomicLoad::new(
            load.type_().clone(),
            rename_expression(load.pointer()),
            load.name(),
        )
        .into(),
        Instruction::AtomicStore(store) => AtomicStore::new(
            store.type_().clone(),
            rename_expression(store.value()),
            rename_expression(store.pointer()),
        )
        .into(),
        Instruction::Call(call) => Call::new(
            call.type_().clone(),
            rename_expression(call.function()),
            call.arguments()
                .iter()
                .map(|argument| rename_expression(argument))
                .collect(),
            call.name(),
        )
        .into(),
        Instruction::CompareAndSwap(cas) => CompareAndSwap::new(
            cas.type_().clone(),
            rename_expression(cas.pointer()),
            rename_expression(cas.old_value()),
            rename_expression(cas.new_value()),
            cas.name(),
        )
        .into(),
        Instruction::ComparisonOperation(operation) => ComparisonOperation::new(
            operation.type_().clone(),
            operation.operator(),
            rename_expression(operation.lhs()),
            rename_expression(operation.rhs()),
            operation.name(),
        )
        .into(),
        Instruction::DeconstructRecord(deconstruct) => DeconstructRecord::new(
            deconstruct.type_().clone(),
            rename_expression(deconstruct.record()),
            deconstruct.element_index(),
            deconstruct.name(),
        )
        .into(),
        Instruction::DeconstructUnion(deconstruct) => DeconstructUnion::new(
            deconstruct.type_().clone(),
            rename_expression(deconstruct.union()),
            deconstruct.member_index(),
            deconstruct.name(),
        )
        .into(),
        Instruction::If(if_) => If::new(
            if_.type_().clone(),
            rename_expression(if_.condition()),
            rename_block(if_.then(), names),
            rename_block(if_.else_(), names),
            if_.name(),
        )
        .into(),
        Instruction::Load(load) => Load::new(
            load.type_().clone(),
            rename_expression(load.pointer()),
            load.name(),
        )
        .into(),
        Instruction::PointerAddress(address) => PointerAddress::new(
            address.type_().clone(),
            rename_expression(address.pointer()),
            rename_expression(address.offset()),
            address.name(),
        )
        .into(),
        Instruction::RecordAddress(address) => RecordAddress::new(
            address.type_().clone(),
            rename_expression(address.pointer()),
            address.element_index(),
            address.name(),
        )
        .into(),
        Instruction::Store(store) => Store::new(
            store.type_().clone(),
            rename_expression(store.value()),
            rename_expression(store.pointer()),
        )
        .into(),
        Instruction::UnionAddress(address) => UnionAddress::new(
            address.type_().clone(),
            rename_expression(address.pointer()),
            address.member_index(),
            address.name(),
        )
        .into(),
        Instruction::AllocateHeap(_) | Instruction::AllocateStack(_) => instruction.clone(),
    }
}

fn rename_terminal_instruction(
    instruction: &TerminalInstruction,
    names: &HashMap<String, String>,
) -> TerminalInstruction {
    match instruction {
        TerminalInstruction::Branch(branch) => Branch::new(
            branch.type_().clone(),
            rename_expression(branch.expression(), names),
        )
        .into(),
        TerminalInstruction::Return(return_) => Return::new(
            return_.type_().clone(),
            rename_expression(return_.expression(), names),
        )
        .into(),
        TerminalInstruction::Unreachable => TerminalInstruction::Unreachable,
    }
}

fn rename_expression(expression: &Expression, names: &HashMap<String, String>) -> Expression {
    match expression {
        Expression::Record(record) => Record::new(
            record.type_().clone(),
            record
                .elements()
                .iter()
                .map(|element| rename_expression(element, names))
                .collect(),
        )
        .into(),
        Expression::Union(union) => Union::new(
            union.type_().clone(),
            union.member_index(),
            rename_expression(union.member(), names),
        )
        .into(),
        Expression::Variable(variable) => Variable::new(rename_name(variable.name(), names)).into(),
        Expression::Primitive(_) | Expression::Undefined(_) => expression.clone(),
    }
}

fn rename_name(name: &str, names: &HashMap<String, String>) -> String {
    names.get(name).cloned().unwrap_or_else(|| name.into())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types;

    #[test]
    fn rename_variable_declaration() {
        let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

        assert_eq!(
            rename_globals(
                &Module::new(
                    vec![VariableDeclaration::new(
                        "x",
                        types::Primitive::PointerInteger
                    )],
                    vec![],
                    vec![],
                    vec![FunctionDefinition::new(
                        "f",
                        vec![],
                        Block::new(
                            vec![],
                            Return::new(pointer_type.clone(), Variable::new("x"))
                        ),
                        pointer_type.clone(),
                        false,
                    )],
                ),
                &vec![("x".into(), "y".into())].into_iter().collect(),
            ),
            Module::new(
                vec![VariableDeclaration::new(
                    "y",
                    types::Primitive::PointerInteger
                )],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    Block::new(
                        vec![],
                        Return::new(pointer_type.clone(), Variable::new("y"))
                    ),
                    pointer_type,
                    false,
                )]
            )
        );
    }

    #[test]
    fn rename_function_declaration() {
        let function_type = types::Function::new(vec![], types::Primitive::PointerInteger);

        assert_eq!(
            rename_globals(
                &Module::new(
                    vec![],
                    vec![FunctionDeclaration::new("x", function_type.clone())],
                    vec![],
                    vec![FunctionDefinition::new(
                        "f",
                        vec![],
                        Block::new(
                            vec![],
                            Return::new(function_type.clone(), Variable::new("x"))
                        ),
                        function_type.clone(),
                        false,
                    )],
                ),
                &vec![("x".into(), "y".into())].into_iter().collect(),
            ),
            Module::new(
                vec![],
                vec![FunctionDeclaration::new("y", function_type.clone(),)],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    Block::new(
                        vec![],
                        Return::new(function_type.clone(), Variable::new("y"))
                    ),
                    function_type.clone(),
                    false,
                )],
            )
        );
    }

    #[test]
    fn rename_variable_definition() {
        let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

        assert_eq!(
            rename_globals(
                &Module::new(
                    vec![],
                    vec![],
                    vec![VariableDefinition::new(
                        "x",
                        Primitive::PointerInteger(0),
                        types::Primitive::PointerInteger,
                        false,
                        false
                    )],
                    vec![FunctionDefinition::new(
                        "f",
                        vec![],
                        Block::new(
                            vec![],
                            Return::new(pointer_type.clone(), Variable::new("x"))
                        ),
                        pointer_type.clone(),
                        false,
                    )]
                ),
                &vec![("x".into(), "y".into())].into_iter().collect(),
            ),
            Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "y",
                    Primitive::PointerInteger(0),
                    types::Primitive::PointerInteger,
                    false,
                    false
                )],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    Block::new(
                        vec![],
                        Return::new(pointer_type.clone(), Variable::new("y"))
                    ),
                    pointer_type,
                    false,
                )]
            )
        );
    }

    #[test]
    fn rename_function_definition() {
        let function_type = types::Function::new(vec![], types::Primitive::PointerInteger);

        assert_eq!(
            rename_globals(
                &Module::new(
                    vec![],
                    vec![],
                    vec![],
                    vec![FunctionDefinition::new(
                        "f",
                        vec![],
                        Block::new(
                            vec![],
                            Return::new(function_type.clone(), Variable::new("f"))
                        ),
                        function_type.clone(),
                        false,
                    )]
                ),
                &vec![("f".into(), "g".into())].into_iter().collect(),
            ),
            Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "g",
                    vec![],
                    Block::new(
                        vec![],
                        Return::new(function_type.clone(), Variable::new("g"))
                    ),
                    function_type,
                    false,
                )]
            )
        );
    }

    #[test]
    fn do_not_rename_local_variable() {
        let function_type = types::Function::new(vec![], types::Primitive::PointerInteger);

        assert_eq!(
            rename_globals(
                &Module::new(
                    vec![],
                    vec![],
                    vec![],
                    vec![FunctionDefinition::new(
                        "f",
                        vec![],
                        Block::new(
                            vec![
                                Call::new(function_type.clone(), Variable::new("f"), vec![], "f")
                                    .into()
                            ],
                            Return::new(types::Primitive::PointerInteger, Variable::new("f"))
                        ),
                        types::Primitive::PointerInteger,
                        false,
                    )]
                ),
                &vec![("f".into(), "g".into())].into_iter().collect(),
            ),
            Module::new(
                vec![],
                vec![],
                vec![],
                vec![FunctionDefinition::new(
                    "g",
                    vec![],
                    Block::new(
                        vec![
                            Call::new(function_type.clone(), Variable::new("g"), vec![], "f")
                                .into()
                        ],
                        Return::new(types::Primitive::PointerInteger, Variable::new("f"))
                    ),
                    types::Primitive::PointerInteger,
                    false,
                )]
            )
        );
    }
}
