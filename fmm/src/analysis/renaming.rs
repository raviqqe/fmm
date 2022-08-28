use crate::ir::*;

pub fn rename(module: &Module, rename: impl Fn(&str) -> String) -> Module {
    Module::new(
        module
            .variable_declarations()
            .iter()
            .map(|declaration| rename_variable_declaration(declaration, &rename))
            .collect(),
        module
            .function_declarations()
            .iter()
            .map(|declaration| rename_function_declaration(declaration, &rename))
            .collect(),
        module
            .variable_definitions()
            .iter()
            .map(|definition| rename_variable_definition(definition, &rename))
            .collect(),
        module
            .function_definitions()
            .iter()
            .map(|definition| rename_function_definition(definition, &rename))
            .collect(),
    )
}

fn rename_variable_declaration(
    declaration: &VariableDeclaration,
    rename: &impl Fn(&str) -> String,
) -> VariableDeclaration {
    VariableDeclaration::new(rename(declaration.name()), declaration.type_().clone())
}

fn rename_function_declaration(
    declaration: &FunctionDeclaration,
    rename: &impl Fn(&str) -> String,
) -> FunctionDeclaration {
    FunctionDeclaration::new(rename(declaration.name()), declaration.type_().clone())
}

fn rename_variable_definition(
    definition: &VariableDefinition,
    rename: &impl Fn(&str) -> String,
) -> VariableDefinition {
    VariableDefinition::new(
        rename(definition.name()),
        rename_expression(definition.body(), rename),
        definition.type_().clone(),
        definition.options().clone(),
    )
}

fn rename_function_definition(
    definition: &FunctionDefinition,
    rename: &impl Fn(&str) -> String,
) -> FunctionDefinition {
    FunctionDefinition::new(
        rename(definition.name()),
        definition
            .arguments()
            .iter()
            .map(|argument| Argument::new(rename(argument.name()), argument.type_().clone()))
            .collect(),
        definition.result_type().clone(),
        rename_block(definition.body(), rename),
        definition.options().clone(),
    )
}

fn rename_block(block: &Block, rename: &impl Fn(&str) -> String) -> Block {
    Block::new(
        block
            .instructions()
            .iter()
            .map(|instruction| rename_instruction(instruction, rename))
            .collect(),
        rename_terminal_instruction(block.terminal_instruction(), rename),
    )
}

fn rename_instruction(instruction: &Instruction, rename: &impl Fn(&str) -> String) -> Instruction {
    let rename_expression = |expression| rename_expression(expression, rename);

    match instruction {
        Instruction::AllocateHeap(allocate) => {
            AllocateHeap::new(rename_expression(allocate.size()), rename(allocate.name())).into()
        }
        Instruction::AllocateStack(allocate) => {
            AllocateStack::new(allocate.type_().clone(), rename(allocate.name())).into()
        }
        Instruction::AtomicLoad(load) => AtomicLoad::new(
            load.type_().clone(),
            rename_expression(load.pointer()),
            load.ordering(),
            rename(load.name()),
        )
        .into(),
        Instruction::AtomicOperation(operation) => AtomicOperation::new(
            operation.type_(),
            operation.operator(),
            rename_expression(operation.pointer()),
            rename_expression(operation.value()),
            operation.ordering(),
            rename(operation.name()),
        )
        .into(),
        Instruction::AtomicStore(store) => AtomicStore::new(
            store.type_().clone(),
            rename_expression(store.value()),
            rename_expression(store.pointer()),
            store.ordering(),
        )
        .into(),
        Instruction::Call(call) => Call::new(
            call.type_().clone(),
            rename_expression(call.function()),
            call.arguments().iter().map(rename_expression).collect(),
            rename(call.name()),
        )
        .into(),
        Instruction::CompareAndSwap(cas) => CompareAndSwap::new(
            cas.type_().clone(),
            rename_expression(cas.pointer()),
            rename_expression(cas.old_value()),
            rename_expression(cas.new_value()),
            cas.success_ordering(),
            cas.failure_ordering(),
            rename(cas.name()),
        )
        .into(),
        Instruction::DeconstructRecord(deconstruct) => DeconstructRecord::new(
            deconstruct.type_().clone(),
            rename_expression(deconstruct.record()),
            deconstruct.field_index(),
            rename(deconstruct.name()),
        )
        .into(),
        Instruction::DeconstructUnion(deconstruct) => DeconstructUnion::new(
            deconstruct.type_().clone(),
            rename_expression(deconstruct.union()),
            deconstruct.member_index(),
            rename(deconstruct.name()),
        )
        .into(),
        Instruction::Fence(fence) => fence.clone().into(),
        Instruction::FreeHeap(free) => FreeHeap::new(rename_expression(free.pointer())).into(),
        Instruction::If(if_) => If::new(
            if_.type_().clone(),
            rename_expression(if_.condition()),
            rename_block(if_.then(), rename),
            rename_block(if_.else_(), rename),
            rename(if_.name()),
        )
        .into(),
        Instruction::Load(load) => Load::new(
            load.type_().clone(),
            rename_expression(load.pointer()),
            rename(load.name()),
        )
        .into(),
        Instruction::ReallocateHeap(reallocate) => ReallocateHeap::new(
            rename_expression(reallocate.pointer()),
            rename_expression(reallocate.size()),
            rename(reallocate.name()),
        )
        .into(),
        Instruction::Store(store) => Store::new(
            store.type_().clone(),
            rename_expression(store.value()),
            rename_expression(store.pointer()),
        )
        .into(),
    }
}

fn rename_terminal_instruction(
    instruction: &TerminalInstruction,
    rename: &impl Fn(&str) -> String,
) -> TerminalInstruction {
    match instruction {
        TerminalInstruction::Branch(branch) => Branch::new(
            branch.type_().clone(),
            rename_expression(branch.expression(), rename),
        )
        .into(),
        TerminalInstruction::Return(return_) => Return::new(
            return_.type_().clone(),
            rename_expression(return_.expression(), rename),
        )
        .into(),
        TerminalInstruction::Unreachable => TerminalInstruction::Unreachable,
    }
}

fn rename_expression(expression: &Expression, rename: &impl Fn(&str) -> String) -> Expression {
    let rename_expression = |expression| rename_expression(expression, rename);

    match expression {
        Expression::ArithmeticOperation(operation) => ArithmeticOperation::new(
            operation.type_(),
            operation.operator(),
            rename_expression(operation.lhs()),
            rename_expression(operation.rhs()),
        )
        .into(),
        Expression::BitCast(bit_cast) => BitCast::new(
            bit_cast.from().clone(),
            bit_cast.to().clone(),
            rename_expression(bit_cast.expression()),
        )
        .into(),
        Expression::BitwiseNotOperation(operation) => {
            BitwiseNotOperation::new(operation.type_(), rename_expression(operation.value())).into()
        }
        Expression::BitwiseOperation(operation) => BitwiseOperation::new(
            operation.type_(),
            operation.operator(),
            rename_expression(operation.lhs()),
            rename_expression(operation.rhs()),
        )
        .into(),
        Expression::ComparisonOperation(operation) => ComparisonOperation::new(
            operation.type_(),
            operation.operator(),
            rename_expression(operation.lhs()),
            rename_expression(operation.rhs()),
        )
        .into(),
        Expression::PointerAddress(address) => PointerAddress::new(
            address.type_().clone(),
            rename_expression(address.pointer()),
            rename_expression(address.offset()),
        )
        .into(),
        Expression::Record(record) => Record::new(
            record.type_().clone(),
            record.fields().iter().map(rename_expression).collect(),
        )
        .into(),
        Expression::RecordAddress(address) => RecordAddress::new(
            address.type_().clone(),
            rename_expression(address.pointer()),
            address.field_index(),
        )
        .into(),
        Expression::Union(union) => Union::new(
            union.type_().clone(),
            union.member_index(),
            rename_expression(union.member()),
        )
        .into(),
        Expression::UnionAddress(address) => UnionAddress::new(
            address.type_().clone(),
            rename_expression(address.pointer()),
            address.member_index(),
        )
        .into(),
        Expression::Variable(variable) => Variable::new(rename(variable.name())).into(),
        Expression::AlignOf(_)
        | Expression::Primitive(_)
        | Expression::SizeOf(_)
        | Expression::Undefined(_) => expression.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{self, CallingConvention, Type};

    fn create_function_type(arguments: Vec<Type>, result: impl Into<Type>) -> types::Function {
        types::Function::new(arguments, result, CallingConvention::Target)
    }

    fn create_function_definition(
        name: impl Into<String>,
        arguments: Vec<Argument>,
        result_type: impl Into<Type>,
        body: Block,
    ) -> FunctionDefinition {
        FunctionDefinition::new(name, arguments, result_type, body, Default::default())
    }

    #[test]
    fn rename_variable_declaration() {
        let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

        assert_eq!(
            rename(
                &Module::new(
                    vec![VariableDeclaration::new(
                        "x",
                        types::Primitive::PointerInteger
                    )],
                    vec![],
                    vec![],
                    vec![create_function_definition(
                        "f",
                        vec![],
                        pointer_type.clone(),
                        Block::new(
                            vec![],
                            Return::new(pointer_type.clone(), Variable::new("x"))
                        ),
                    )],
                ),
                |name| if name == "x" { "y".into() } else { name.into() },
            ),
            Module::new(
                vec![VariableDeclaration::new(
                    "y",
                    types::Primitive::PointerInteger
                )],
                vec![],
                vec![],
                vec![create_function_definition(
                    "f",
                    vec![],
                    pointer_type.clone(),
                    Block::new(vec![], Return::new(pointer_type, Variable::new("y"))),
                )]
            )
        );
    }

    #[test]
    fn rename_function_declaration() {
        let function_type = create_function_type(vec![], types::Primitive::PointerInteger);

        assert_eq!(
            rename(
                &Module::new(
                    vec![],
                    vec![FunctionDeclaration::new("x", function_type.clone())],
                    vec![],
                    vec![create_function_definition(
                        "f",
                        vec![],
                        function_type.clone(),
                        Block::new(
                            vec![],
                            Return::new(function_type.clone(), Variable::new("x"))
                        ),
                    )],
                ),
                |name| if name == "x" { "y".into() } else { name.into() },
            ),
            Module::new(
                vec![],
                vec![FunctionDeclaration::new("y", function_type.clone(),)],
                vec![],
                vec![create_function_definition(
                    "f",
                    vec![],
                    function_type.clone(),
                    Block::new(vec![], Return::new(function_type, Variable::new("y"))),
                )],
            )
        );
    }

    #[test]
    fn rename_variable_definition() {
        let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

        assert_eq!(
            rename(
                &Module::new(
                    vec![],
                    vec![],
                    vec![VariableDefinition::new(
                        "x",
                        Primitive::PointerInteger(0),
                        types::Primitive::PointerInteger,
                        Default::default(),
                    )],
                    vec![create_function_definition(
                        "f",
                        vec![],
                        pointer_type.clone(),
                        Block::new(
                            vec![],
                            Return::new(pointer_type.clone(), Variable::new("x"))
                        ),
                    )]
                ),
                |name| if name == "x" { "y".into() } else { name.into() },
            ),
            Module::new(
                vec![],
                vec![],
                vec![VariableDefinition::new(
                    "y",
                    Primitive::PointerInteger(0),
                    types::Primitive::PointerInteger,
                    Default::default(),
                )],
                vec![create_function_definition(
                    "f",
                    vec![],
                    pointer_type.clone(),
                    Block::new(vec![], Return::new(pointer_type, Variable::new("y"))),
                )]
            )
        );
    }

    #[test]
    fn rename_function_definition() {
        let function_type = create_function_type(vec![], types::Primitive::PointerInteger);

        assert_eq!(
            rename(
                &Module::new(
                    vec![],
                    vec![],
                    vec![],
                    vec![create_function_definition(
                        "f",
                        vec![],
                        function_type.clone(),
                        Block::new(
                            vec![],
                            Return::new(function_type.clone(), Variable::new("f"))
                        ),
                    )]
                ),
                |name| if name == "f" { "g".into() } else { name.into() },
            ),
            Module::new(
                vec![],
                vec![],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![],
                    function_type.clone(),
                    Block::new(vec![], Return::new(function_type, Variable::new("g"))),
                )]
            )
        );
    }

    #[test]
    fn rename_local_variable() {
        let function_type = create_function_type(vec![], types::Primitive::PointerInteger);

        assert_eq!(
            rename(
                &Module::new(
                    vec![],
                    vec![],
                    vec![],
                    vec![create_function_definition(
                        "f",
                        vec![],
                        types::Primitive::PointerInteger,
                        Block::new(
                            vec![
                                Call::new(function_type.clone(), Variable::new("f"), vec![], "f")
                                    .into()
                            ],
                            Return::new(types::Primitive::PointerInteger, Variable::new("f"))
                        ),
                    )]
                ),
                |name| if name == "f" { "g".into() } else { name.into() },
            ),
            Module::new(
                vec![],
                vec![],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![Call::new(function_type, Variable::new("g"), vec![], "g").into()],
                        Return::new(types::Primitive::PointerInteger, Variable::new("g"))
                    ),
                )]
            )
        );
    }

    #[test]
    fn rename_arguments_in_function_definition() {
        assert_eq!(
            rename(
                &Module::new(
                    vec![],
                    vec![],
                    vec![],
                    vec![create_function_definition(
                        "f",
                        vec![Argument::new("x", types::Primitive::PointerInteger)],
                        types::Primitive::PointerInteger,
                        Block::new(
                            vec![],
                            Return::new(types::Primitive::PointerInteger, Variable::new("x"))
                        ),
                    )]
                ),
                |name| if name == "x" { "y".into() } else { name.into() },
            ),
            Module::new(
                vec![],
                vec![],
                vec![],
                vec![create_function_definition(
                    "f",
                    vec![Argument::new("y", types::Primitive::PointerInteger)],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![],
                        Return::new(types::Primitive::PointerInteger, Variable::new("y"))
                    ),
                )]
            )
        );
    }
}
