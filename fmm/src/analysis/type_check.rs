mod error;

use super::local_variable;
use crate::{
    ir::*,
    types::{self, generic_pointer_type, Type},
};
pub use error::*;
use fnv::FnvHashMap;

pub fn check(module: &Module) -> Result<(), TypeCheckError> {
    check_variable_declarations(module)?;
    check_function_declarations(module)?;

    let mut variables = module
        .variable_declarations()
        .iter()
        .map(|declaration| {
            (
                declaration.name(),
                types::Pointer::new(declaration.type_().clone()).into(),
            )
        })
        .chain(
            module
                .function_declarations()
                .iter()
                .map(|declaration| (declaration.name(), declaration.type_().clone().into())),
        )
        .chain(module.variable_definitions().iter().map(|definition| {
            (
                definition.name(),
                types::Pointer::new(definition.type_().clone()).into(),
            )
        }))
        .chain(
            module
                .function_definitions()
                .iter()
                .map(|definition| (definition.name(), definition.type_().into())),
        )
        .collect::<FnvHashMap<_, _>>();

    for definition in module.variable_definitions() {
        check_variable_definition(definition, &variables)?;
    }

    for definition in module.function_definitions() {
        check_function_definition(definition, &mut variables)?;
    }

    Ok(())
}

fn check_variable_declarations(module: &Module) -> Result<(), TypeCheckError> {
    let variables = module
        .variable_declarations()
        .iter()
        .map(|declaration| (declaration.name(), declaration.type_().clone()))
        .collect::<FnvHashMap<_, _>>();

    for definition in module.variable_definitions() {
        if let Some(type_) = variables.get(definition.name()) {
            check_equality(definition.type_(), type_)?;
        }
    }

    Ok(())
}

fn check_function_declarations(module: &Module) -> Result<(), TypeCheckError> {
    let functions = module
        .function_declarations()
        .iter()
        .map(|declaration| (declaration.name(), declaration.type_().clone()))
        .collect::<FnvHashMap<_, _>>();

    for definition in module.function_definitions() {
        if let Some(type_) = functions.get(definition.name()) {
            check_equality(&definition.type_().clone().into(), &type_.clone().into())?;
        }
    }

    Ok(())
}

fn check_variable_definition(
    definition: &VariableDefinition,
    variables: &FnvHashMap<&str, Type>,
) -> Result<(), TypeCheckError> {
    check_equality(
        &check_expression(definition.body(), variables)?,
        definition.type_(),
    )
}

fn check_function_definition<'a>(
    definition: &'a FunctionDefinition,
    variables: &mut FnvHashMap<&'a str, Type>,
) -> Result<(), TypeCheckError> {
    let local_variables = local_variable::collect(definition);

    variables.extend(local_variables.clone());

    check_block(definition.body(), definition.result_type(), None, variables)?;

    for name in local_variables.keys() {
        variables.remove(name);
    }

    Ok(())
}

fn check_block(
    block: &Block,
    result_type: &Type,
    branch_type: Option<&Type>,
    variables: &FnvHashMap<&str, Type>,
) -> Result<(), TypeCheckError> {
    for instruction in block.instructions() {
        match instruction {
            Instruction::AllocateHeap(allocate) => {
                check_equality(
                    &check_expression(allocate.size(), variables)?,
                    &types::Primitive::PointerInteger.into(),
                )?;
            }
            Instruction::AllocateStack(_) => {}
            Instruction::AtomicLoad(load) => {
                check_equality(
                    &check_expression(load.pointer(), variables)?,
                    &types::Pointer::new(load.type_().clone()).clone().into(),
                )?;
            }
            Instruction::AtomicOperation(operation) => {
                check_equality(
                    &check_expression(operation.pointer(), variables)?,
                    &types::Pointer::new(operation.type_()).into(),
                )?;
                check_equality(
                    &check_expression(operation.value(), variables)?,
                    &operation.type_().into(),
                )?;
            }
            Instruction::AtomicStore(store) => {
                check_equality(
                    &check_expression(store.value(), variables)?,
                    &store.type_().clone(),
                )?;
                check_equality(
                    &check_expression(store.pointer(), variables)?,
                    &types::Pointer::new(store.type_().clone()).into(),
                )?;
            }
            Instruction::Call(call) => {
                if call.arguments().len() != call.type_().arguments().len() {
                    return Err(TypeCheckError::FunctionArguments(call.clone()));
                }

                check_equality(
                    &call.type_().clone().into(),
                    &check_expression(call.function(), variables)?,
                )?;

                for (argument, type_) in call.arguments().iter().zip(call.type_().arguments()) {
                    check_equality(&check_expression(argument, variables)?, type_)?;
                }
            }
            Instruction::CompareAndSwap(cas) => {
                check_equality(
                    &check_expression(cas.pointer(), variables)?,
                    &types::Pointer::new(cas.type_().clone()).into(),
                )?;

                check_equality(
                    &check_expression(cas.old_value(), variables)?,
                    &cas.type_().clone(),
                )?;

                check_equality(
                    &check_expression(cas.new_value(), variables)?,
                    &cas.type_().clone(),
                )?;
            }
            Instruction::DeconstructRecord(deconstruct) => {
                check_equality(
                    &check_expression(deconstruct.record(), variables)?,
                    &deconstruct.type_().clone().into(),
                )?;

                check_record_index(deconstruct.field_index(), deconstruct.type_())?;
            }
            Instruction::DeconstructUnion(deconstruct) => {
                check_equality(
                    &check_expression(deconstruct.union(), variables)?,
                    &deconstruct.type_().clone().into(),
                )?;

                check_union_index(deconstruct.member_index(), deconstruct.type_())?;
            }
            Instruction::Fence(_) => {}
            Instruction::FreeHeap(free) => {
                check_equality(
                    &check_expression(free.pointer(), variables)?,
                    &generic_pointer_type(),
                )?;
            }
            Instruction::If(if_) => {
                check_equality(
                    &check_expression(if_.condition(), variables)?,
                    &types::Primitive::Boolean.into(),
                )?;

                check_block(if_.then(), result_type, Some(if_.type_()), variables)?;
                check_block(if_.else_(), result_type, Some(if_.type_()), variables)?;
            }
            Instruction::Load(load) => {
                check_equality(
                    &check_expression(load.pointer(), variables)?,
                    &types::Pointer::new(load.type_().clone()).into(),
                )?;
            }
            Instruction::MemoryCopy(copy) => {
                let pointer_type = types::Pointer::new(types::Primitive::Integer8).into();

                check_equality(&check_expression(copy.source(), variables)?, &pointer_type)?;
                check_equality(
                    &check_expression(copy.destination(), variables)?,
                    &pointer_type,
                )?;
                check_equality(
                    &check_expression(copy.size(), variables)?,
                    &types::Primitive::PointerInteger.into(),
                )?;
            }
            Instruction::ReallocateHeap(reallocate) => {
                check_equality(
                    &check_expression(reallocate.pointer(), variables)?,
                    &generic_pointer_type(),
                )?;

                check_equality(
                    &check_expression(reallocate.size(), variables)?,
                    &types::Primitive::PointerInteger.into(),
                )?;
            }
            Instruction::Store(store) => {
                check_equality(&check_expression(store.value(), variables)?, store.type_())?;
                check_equality(
                    &check_expression(store.pointer(), variables)?,
                    &types::Pointer::new(store.type_().clone()).into(),
                )?;
            }
        }
    }

    match block.terminal_instruction() {
        TerminalInstruction::Branch(branch) => {
            let branch_type =
                branch_type.ok_or_else(|| TypeCheckError::InvalidBranch(branch.clone()))?;

            check_equality(branch.type_(), branch_type)?;
            check_equality(
                &check_expression(branch.expression(), variables)?,
                branch_type,
            )?;
        }
        TerminalInstruction::Return(return_) => {
            check_equality(return_.type_(), result_type)?;
            check_equality(
                &check_expression(return_.expression(), variables)?,
                result_type,
            )?;
        }
        TerminalInstruction::Unreachable => {}
    }

    Ok(())
}

fn check_expression(
    expression: &Expression,
    variables: &FnvHashMap<&str, Type>,
) -> Result<Type, TypeCheckError> {
    Ok(match expression {
        Expression::AlignOf(_) => AlignOf::RESULT_TYPE.into(),
        Expression::ArithmeticOperation(operation) => {
            check_equality(
                &check_expression(operation.lhs(), variables)?,
                &operation.type_().into(),
            )?;
            check_equality(
                &check_expression(operation.rhs(), variables)?,
                &operation.type_().into(),
            )?;

            operation.type_().into()
        }
        Expression::BitCast(bit_cast) => {
            check_equality(
                &check_expression(bit_cast.expression(), variables)?,
                bit_cast.from(),
            )?;

            bit_cast.to().clone()
        }
        Expression::BitwiseNotOperation(operation) => {
            check_equality(
                &check_expression(operation.value(), variables)?,
                &operation.type_().into(),
            )?;

            operation.type_().into()
        }
        Expression::BitwiseOperation(operation) => {
            check_equality(
                &check_expression(operation.lhs(), variables)?,
                &operation.type_().into(),
            )?;
            check_equality(
                &check_expression(operation.rhs(), variables)?,
                &operation.type_().into(),
            )?;

            operation.type_().into()
        }
        Expression::ComparisonOperation(operation) => {
            check_equality(
                &check_expression(operation.lhs(), variables)?,
                &operation.type_().into(),
            )?;
            check_equality(
                &check_expression(operation.rhs(), variables)?,
                &operation.type_().into(),
            )?;

            ComparisonOperation::RESULT_TYPE.into()
        }
        Expression::PointerAddress(address) => {
            check_equality(
                &check_expression(address.pointer(), variables)?,
                &address.type_().clone().into(),
            )?;

            check_equality(
                &check_expression(address.offset(), variables)?,
                &types::Primitive::PointerInteger.into(),
            )?;

            address.type_().clone().into()
        }
        Expression::Primitive(primitive) => primitive.type_().into(),
        Expression::Record(record) => {
            if record.fields().len() != record.type_().fields().len() {
                return Err(TypeCheckError::RecordFields(record.clone()));
            }

            for (field, type_) in record.fields().iter().zip(record.type_().fields()) {
                check_equality(&check_expression(field, variables)?, type_)?;
            }

            record.type_().clone().into()
        }
        Expression::RecordAddress(address) => {
            check_equality(
                &check_expression(address.pointer(), variables)?,
                &types::Pointer::new(address.type_().clone()).into(),
            )?;

            check_record_index(address.field_index(), address.type_())?;

            types::Pointer::new(address.type_().fields()[address.field_index()].clone()).into()
        }
        Expression::SizeOf(_) => SizeOf::RESULT_TYPE.into(),
        Expression::Undefined(undefined) => undefined.type_().clone(),
        Expression::Union(union) => {
            check_equality(
                &check_expression(union.member(), variables)?,
                union
                    .type_()
                    .members()
                    .get(union.member_index())
                    .ok_or(TypeCheckError::IndexOutOfRange)?,
            )?;

            union.type_().clone().into()
        }
        Expression::UnionAddress(address) => {
            check_equality(
                &check_expression(address.pointer(), variables)?,
                &types::Pointer::new(address.type_().clone()).into(),
            )?;

            check_union_index(address.member_index(), address.type_())?;

            types::Pointer::new(address.type_().members()[address.member_index()].clone()).into()
        }
        Expression::Variable(variable) => variables
            .get(variable.name())
            .cloned()
            .ok_or_else(|| TypeCheckError::VariableNotFound(variable.clone()))?,
    })
}

fn check_record_index(index: usize, type_: &types::Record) -> Result<(), TypeCheckError> {
    if index < type_.fields().len() {
        Ok(())
    } else {
        Err(TypeCheckError::IndexOutOfRange)
    }
}

fn check_union_index(index: usize, type_: &types::Union) -> Result<(), TypeCheckError> {
    if index < type_.members().len() {
        Ok(())
    } else {
        Err(TypeCheckError::IndexOutOfRange)
    }
}

fn check_equality(one: &Type, other: &Type) -> Result<(), TypeCheckError> {
    if one == other {
        Ok(())
    } else {
        Err(TypeCheckError::TypesNotMatched(one.clone(), other.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::CallingConvention;

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

    #[test]
    fn check_empty_module() -> Result<(), TypeCheckError> {
        check(&Module::new(vec![], vec![], vec![], vec![]))
    }

    #[test]
    fn check_variable_declaration() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![VariableDeclaration::new(
                "x",
                types::Primitive::PointerInteger,
            )],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![
                        Load::new(types::Primitive::PointerInteger, Variable::new("x"), "y").into(),
                    ],
                    Return::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(42),
                    ),
                ),
            )],
        ))
    }

    #[test]
    fn check_variable_definition() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![],
            vec![],
            vec![VariableDefinition::new(
                "x",
                Primitive::PointerInteger(42),
                types::Primitive::PointerInteger,
                Default::default(),
            )],
            vec![create_function_definition(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![
                        Load::new(types::Primitive::PointerInteger, Variable::new("x"), "y").into(),
                    ],
                    Return::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(42),
                    ),
                ),
            )],
        ))
    }

    #[test]
    fn check_function_declaration() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![],
            vec![FunctionDeclaration::new(
                "f",
                create_function_type(
                    vec![types::Primitive::PointerInteger.into()],
                    types::Primitive::Float64,
                ),
            )],
            vec![],
            vec![create_function_definition(
                "g",
                vec![Argument::new("x", types::Primitive::PointerInteger)],
                types::Primitive::Float64,
                Block::new(
                    vec![Call::new(
                        create_function_type(
                            vec![types::Primitive::PointerInteger.into()],
                            types::Primitive::Float64,
                        ),
                        Variable::new("f"),
                        vec![Primitive::PointerInteger(42).into()],
                        "x",
                    )
                    .into()],
                    Return::new(types::Primitive::Float64, Variable::new("x")),
                ),
            )],
        ))
    }

    #[test]
    fn check_return() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![],
                    Return::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(42),
                    ),
                ),
            )],
        ))
    }

    #[test]
    fn check_allocate_heap() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![Argument::new("x", types::Primitive::PointerInteger)],
                generic_pointer_type(),
                Block::new(
                    vec![AllocateHeap::new(Primitive::PointerInteger(42), "x").into()],
                    Return::new(generic_pointer_type(), Variable::new("x")),
                ),
            )],
        ))
    }

    #[test]
    fn check_reallocate_heap() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![Argument::new("x", generic_pointer_type())],
                generic_pointer_type(),
                Block::new(
                    vec![ReallocateHeap::new(
                        Variable::new("x"),
                        Primitive::PointerInteger(42),
                        "y",
                    )
                    .into()],
                    Return::new(generic_pointer_type(), Variable::new("y")),
                ),
            )],
        ))
    }

    #[test]
    fn check_allocate_stack() -> Result<(), TypeCheckError> {
        let pointer_type = types::Pointer::new(types::Primitive::Float64);

        check(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![Argument::new("x", types::Primitive::PointerInteger)],
                pointer_type.clone(),
                Block::new(
                    vec![AllocateStack::new(types::Primitive::Float64, "x").into()],
                    Return::new(pointer_type, Variable::new("x")),
                ),
            )],
        ))
    }

    #[test]
    fn check_call() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![Argument::new("x", types::Primitive::PointerInteger)],
                types::Primitive::Float64,
                Block::new(
                    vec![Call::new(
                        create_function_type(
                            vec![types::Primitive::PointerInteger.into()],
                            types::Primitive::Float64,
                        ),
                        Variable::new("f"),
                        vec![Primitive::PointerInteger(42).into()],
                        "x",
                    )
                    .into()],
                    Return::new(types::Primitive::Float64, Variable::new("x")),
                ),
            )],
        ))
    }

    #[test]
    #[should_panic]
    fn fail_to_check_call_with_wrong_function_type() {
        check(&Module::new(
            vec![],
            vec![FunctionDeclaration::new(
                "g",
                create_function_type(
                    vec![types::Primitive::Float64.into()],
                    types::Primitive::Float64,
                ),
            )],
            vec![],
            vec![create_function_definition(
                "f",
                vec![Argument::new("x", types::Primitive::PointerInteger)],
                types::Primitive::Float64,
                Block::new(
                    vec![Call::new(
                        create_function_type(
                            vec![types::Primitive::PointerInteger.into()],
                            types::Primitive::Float64,
                        ),
                        Variable::new("g"),
                        vec![Primitive::PointerInteger(42).into()],
                        "x",
                    )
                    .into()],
                    Return::new(types::Primitive::Float64, Variable::new("x")),
                ),
            )],
        ))
        .unwrap()
    }

    #[test]
    fn check_if() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![],
                types::Primitive::Float64,
                Block::new(
                    vec![If::new(
                        types::Primitive::Float64,
                        Primitive::Boolean(true),
                        Block::new(
                            vec![],
                            Branch::new(types::Primitive::Float64, Primitive::Float64(42.0)),
                        ),
                        Block::new(
                            vec![],
                            Branch::new(types::Primitive::Float64, Primitive::Float64(42.0)),
                        ),
                        "x",
                    )
                    .into()],
                    Return::new(types::Primitive::Float64, Variable::new("x")),
                ),
            )],
        ))
    }

    #[test]
    fn check_load() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![
                        Load::new(types::Primitive::PointerInteger, Variable::new("x"), "y").into(),
                    ],
                    Return::new(types::Primitive::PointerInteger, Variable::new("y")),
                ),
            )],
        ))
    }

    #[test]
    fn check_memory_copy() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![
                    Argument::new("x", types::Pointer::new(types::Primitive::Integer8)),
                    Argument::new("y", types::Pointer::new(types::Primitive::Integer8)),
                ],
                types::void_type(),
                Block::new(
                    vec![MemoryCopy::new(
                        Variable::new("x"),
                        Variable::new("y"),
                        Primitive::PointerInteger(42),
                    )
                    .into()],
                    Return::new(types::void_type(), void_value()),
                ),
            )],
        ))
    }

    #[test]
    fn check_store() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![Store::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(42),
                        Variable::new("x"),
                    )
                    .into()],
                    Return::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(42),
                    ),
                ),
            )],
        ))
    }

    #[test]
    fn check_atomic_load() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
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
            )],
        ))
    }

    #[test]
    fn check_atomic_store() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![AtomicStore::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(42),
                        Variable::new("x"),
                        AtomicOrdering::Relaxed,
                    )
                    .into()],
                    Return::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(42),
                    ),
                ),
            )],
        ))
    }

    #[test]
    fn check_pointer_address() -> Result<(), TypeCheckError> {
        let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

        check(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![Argument::new("x", pointer_type.clone())],
                pointer_type.clone(),
                Block::new(
                    vec![],
                    Return::new(
                        pointer_type.clone(),
                        PointerAddress::new(
                            pointer_type,
                            Variable::new("x"),
                            Primitive::PointerInteger(42),
                        ),
                    ),
                ),
            )],
        ))
    }

    #[test]
    fn check_record_address() -> Result<(), TypeCheckError> {
        let record_type = types::Record::new(vec![types::Primitive::PointerInteger.into()]);

        check(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![Argument::new("x", types::Pointer::new(record_type.clone()))],
                types::Pointer::new(types::Primitive::PointerInteger),
                Block::new(
                    vec![],
                    Return::new(
                        types::Pointer::new(types::Primitive::PointerInteger),
                        RecordAddress::new(record_type, Variable::new("x"), 0),
                    ),
                ),
            )],
        ))
    }

    #[test]
    fn check_union_address() -> Result<(), TypeCheckError> {
        let union_type = types::Union::new(vec![types::Primitive::PointerInteger.into()]);

        check(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![Argument::new("x", types::Pointer::new(union_type.clone()))],
                types::Pointer::new(types::Primitive::PointerInteger),
                Block::new(
                    vec![],
                    Return::new(
                        types::Pointer::new(types::Primitive::PointerInteger),
                        UnionAddress::new(union_type, Variable::new("x"), 0),
                    ),
                ),
            )],
        ))
    }

    #[test]
    fn check_align_of() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![],
            vec![],
            vec![VariableDefinition::new(
                "x",
                AlignOf::new(types::Primitive::Float64),
                types::Primitive::PointerInteger,
                Default::default(),
            )],
            vec![],
        ))
    }

    #[test]
    fn check_size_of() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![],
            vec![],
            vec![VariableDefinition::new(
                "x",
                SizeOf::new(types::Primitive::Float64),
                types::Primitive::PointerInteger,
                Default::default(),
            )],
            vec![],
        ))
    }

    #[test]
    fn check_bit_cast() -> Result<(), TypeCheckError> {
        check(&Module::new(
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
        ))
    }

    #[test]
    fn check_atomic_operation() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
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
            )],
        ))
    }

    #[test]
    fn check_free_heap() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![Argument::new("x", generic_pointer_type())],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![FreeHeap::new(Variable::new("x")).into()],
                    Return::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(0),
                    ),
                ),
            )],
        ))
    }

    #[test]
    fn check_bitwise_operation() -> Result<(), TypeCheckError> {
        check(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![],
                    Return::new(
                        types::Primitive::PointerInteger,
                        BitwiseOperation::new(
                            types::Primitive::PointerInteger,
                            BitwiseOperator::And,
                            Primitive::PointerInteger(0),
                            Primitive::PointerInteger(1),
                        ),
                    ),
                ),
            )],
        ))
    }

    #[test]
    fn check_variable_declaration_matching_definition() {
        assert!(matches!(
            check(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Primitive::PointerInteger,
                )],
                vec![],
                vec![VariableDefinition::new(
                    "x",
                    Primitive::PointerInteger(42),
                    types::Primitive::Integer64,
                    Default::default(),
                )],
                vec![],
            )),
            Err(TypeCheckError::TypesNotMatched(..))
        ));
    }

    #[test]
    fn check_function_declaration_matching_definition() {
        assert!(matches!(
            check(&Module::new(
                vec![],
                vec![FunctionDeclaration::new(
                    "f",
                    types::Function::new(
                        vec![],
                        types::Primitive::PointerInteger,
                        CallingConvention::Source
                    )
                )],
                vec![],
                vec![create_function_definition(
                    "f",
                    vec![],
                    types::Primitive::Integer64,
                    Block::new(vec![], TerminalInstruction::Unreachable),
                )],
            )),
            Err(TypeCheckError::TypesNotMatched(..))
        ));
    }
}
