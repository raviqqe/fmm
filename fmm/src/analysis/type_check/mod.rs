mod error;

use crate::ir::*;
use crate::types::{self, Type};
pub use error::*;
use std::collections::HashMap;

pub fn check_types(module: &Module) -> Result<(), TypeCheckError> {
    let variables = module
        .variable_declarations()
        .iter()
        .map(|declaration| {
            (
                declaration.name().into(),
                types::Pointer::new(declaration.type_().clone()).into(),
            )
        })
        .chain(module.function_declarations().iter().map(|declaration| {
            (
                declaration.name().into(),
                declaration.type_().clone().into(),
            )
        }))
        .chain(module.variable_definitions().iter().map(|definition| {
            (
                definition.name().into(),
                types::Pointer::new(definition.type_().clone()).into(),
            )
        }))
        .chain(
            module
                .function_definitions()
                .iter()
                .map(|definition| (definition.name().into(), definition.type_().clone().into())),
        )
        .collect::<HashMap<String, Type>>();

    for definition in module.variable_definitions() {
        check_variable_definition(definition, &variables)?;
    }

    for definition in module.function_definitions() {
        check_function_definition(definition, &variables)?;
    }

    Ok(())
}

fn check_variable_definition(
    definition: &VariableDefinition,
    variables: &HashMap<String, Type>,
) -> Result<(), TypeCheckError> {
    check_equality(
        &check_expression(definition.body(), &variables)?,
        definition.type_(),
    )
}

fn check_function_definition(
    definition: &FunctionDefinition,
    variables: &HashMap<String, Type>,
) -> Result<(), TypeCheckError> {
    check_block(
        definition.body(),
        &variables
            .iter()
            .map(|(name, type_)| (name.clone(), type_.clone()))
            .chain(
                definition
                    .arguments()
                    .iter()
                    .map(|argument| (argument.name().into(), argument.type_().clone())),
            )
            .collect(),
        definition.result_type(),
        None,
    )?;

    Ok(())
}

fn check_block(
    block: &Block,
    variables: &HashMap<String, Type>,
    return_type: &Type,
    branch_type: Option<&Type>,
) -> Result<(), TypeCheckError> {
    let mut variables = variables.clone();

    for instruction in block.instructions() {
        match instruction {
            Instruction::AllocateHeap(allocate) => {
                variables.insert(
                    allocate.name().into(),
                    types::Pointer::new(allocate.type_().clone()).into(),
                );
            }
            Instruction::ArithmeticOperation(operation) => {
                check_equality(
                    &check_expression(operation.lhs(), &variables)?,
                    &operation.type_().into(),
                )?;
                check_equality(
                    &check_expression(operation.rhs(), &variables)?,
                    &operation.type_().into(),
                )?;

                variables.insert(operation.name().into(), operation.type_().clone().into());
            }
            Instruction::AtomicLoad(load) => {
                check_equality(
                    &check_expression(load.pointer(), &variables)?,
                    &types::Pointer::new(load.type_().clone()).clone().into(),
                )?;

                variables.insert(load.name().into(), load.type_().clone());
            }
            Instruction::AtomicStore(store) => {
                check_equality(
                    &check_expression(store.value(), &variables)?,
                    &store.type_().clone(),
                )?;
                check_equality(
                    &check_expression(store.pointer(), &variables)?,
                    &types::Pointer::new(store.type_().clone()).into(),
                )?;
            }
            Instruction::Call(call) => {
                if call.arguments().len() != call.type_().arguments().len() {
                    return Err(TypeCheckError::FunctionArguments(call.clone()));
                }

                for (argument, type_) in call.arguments().iter().zip(call.type_().arguments()) {
                    check_equality(&check_expression(argument, &variables)?, type_)?;
                }

                variables.insert(call.name().into(), call.type_().result().clone());
            }
            Instruction::CompareAndSwap(cas) => {
                check_equality(
                    &check_expression(cas.pointer(), &variables)?,
                    &types::Pointer::new(cas.type_().clone()).into(),
                )?;

                check_equality(
                    &check_expression(cas.old_value(), &variables)?,
                    &cas.type_().clone(),
                )?;

                check_equality(
                    &check_expression(cas.new_value(), &variables)?,
                    &cas.type_().clone(),
                )?;

                variables.insert(cas.name().into(), types::Primitive::Bool.into());
            }
            Instruction::ComparisonOperation(operation) => {
                check_equality(
                    &check_expression(operation.lhs(), &variables)?,
                    &operation.type_().into(),
                )?;
                check_equality(
                    &check_expression(operation.rhs(), &variables)?,
                    &operation.type_().into(),
                )?;

                variables.insert(operation.name().into(), types::Primitive::Bool.into());
            }
            Instruction::DeconstructRecord(deconstruct) => {
                check_equality(
                    &check_expression(deconstruct.record(), &variables)?,
                    &deconstruct.type_().clone().into(),
                )?;

                variables.insert(
                    deconstruct.name().into(),
                    deconstruct
                        .type_()
                        .elements()
                        .get(deconstruct.element_index())
                        .ok_or(TypeCheckError::IndexOutOfRange)?
                        .clone(),
                );
            }
            Instruction::DeconstructUnion(deconstruct) => {
                check_equality(
                    &check_expression(deconstruct.union(), &variables)?,
                    &deconstruct.type_().clone().into(),
                )?;

                variables.insert(
                    deconstruct.name().into(),
                    deconstruct
                        .type_()
                        .members()
                        .get(deconstruct.member_index())
                        .ok_or(TypeCheckError::IndexOutOfRange)?
                        .clone(),
                );
            }
            Instruction::If(if_) => {
                check_equality(
                    &check_expression(if_.condition(), &variables)?,
                    &types::Primitive::Bool.into(),
                )?;

                check_block(if_.then(), &variables, return_type, Some(if_.type_()))?;
                check_block(if_.else_(), &variables, return_type, Some(if_.type_()))?;

                variables.insert(if_.name().into(), if_.type_().clone());
            }
            Instruction::Load(load) => {
                check_equality(
                    &check_expression(load.pointer(), &variables)?,
                    &types::Pointer::new(load.type_().clone()).into(),
                )?;

                variables.insert(load.name().into(), load.type_().clone());
            }
            Instruction::PointerAddress(address) => {
                check_equality(
                    &check_expression(address.pointer(), &variables)?,
                    &address.type_().clone().into(),
                )?;

                check_equality(
                    &check_expression(address.offset(), &variables)?,
                    &types::Primitive::PointerInteger.into(),
                )?;

                variables.insert(address.name().into(), address.type_().clone().into());
            }
            Instruction::RecordAddress(address) => {
                check_equality(
                    &check_expression(address.pointer(), &variables)?,
                    &types::Pointer::new(address.type_().clone()).into(),
                )?;

                variables.insert(
                    address.name().into(),
                    types::Pointer::new(
                        address
                            .type_()
                            .elements()
                            .get(address.element_index())
                            .ok_or(TypeCheckError::IndexOutOfRange)?
                            .clone(),
                    )
                    .into(),
                );
            }
            Instruction::Store(store) => {
                check_equality(&check_expression(store.value(), &variables)?, store.type_())?;
                check_equality(
                    &check_expression(store.pointer(), &variables)?,
                    &types::Pointer::new(store.type_().clone()).into(),
                )?;
            }
            Instruction::UnionAddress(address) => {
                check_equality(
                    &check_expression(address.pointer(), &variables)?,
                    &types::Pointer::new(address.type_().clone()).into(),
                )?;

                variables.insert(
                    address.name().into(),
                    types::Pointer::new(
                        address
                            .type_()
                            .members()
                            .get(address.member_index())
                            .ok_or(TypeCheckError::IndexOutOfRange)?
                            .clone(),
                    )
                    .into(),
                );
            }
        }
    }

    match block.terminal_instruction() {
        TerminalInstruction::Branch(branch) => {
            let branch_type =
                branch_type.ok_or_else(|| TypeCheckError::InvalidBranch(branch.clone()))?;

            check_equality(branch.type_(), branch_type)?;
            check_equality(
                &check_expression(branch.expression(), &variables)?,
                branch_type,
            )?;
        }
        TerminalInstruction::Return(return_) => {
            check_equality(return_.type_(), return_type)?;
            check_equality(
                &check_expression(return_.expression(), &variables)?,
                return_type,
            )?;
        }
        TerminalInstruction::Unreachable => {}
    }

    Ok(())
}

fn check_expression(
    expression: &Expression,
    variables: &HashMap<String, Type>,
) -> Result<Type, TypeCheckError> {
    Ok(match expression {
        Expression::Primitive(primitive) => check_primitive(*primitive).into(),
        Expression::Record(record) => {
            if record.elements().len() != record.type_().elements().len() {
                return Err(TypeCheckError::RecordElements(record.clone()));
            }

            for (element, type_) in record.elements().iter().zip(record.type_().elements()) {
                check_equality(&check_expression(element, variables)?, &type_)?;
            }

            record.type_().clone().into()
        }
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
        Expression::Variable(variable) => variables
            .get(variable.name())
            .cloned()
            .ok_or_else(|| TypeCheckError::VariableNotFound(variable.clone()))?,
        Expression::Undefined(undefined) => undefined.type_().clone(),
    })
}

fn check_primitive(primitive: Primitive) -> types::Primitive {
    match primitive {
        Primitive::Bool(_) => types::Primitive::Bool,
        Primitive::Float32(_) => types::Primitive::Float32,
        Primitive::Float64(_) => types::Primitive::Float64,
        Primitive::Integer8(_) => types::Primitive::Integer8,
        Primitive::Integer32(_) => types::Primitive::Integer32,
        Primitive::Integer64(_) => types::Primitive::Integer64,
        Primitive::PointerInteger(_) => types::Primitive::PointerInteger,
    }
}

fn check_equality(one: &Type, other: &Type) -> Result<(), TypeCheckError> {
    if one != other {
        return Err(TypeCheckError::TypesNotMatched(one.clone(), other.clone()));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_empty_module() -> Result<(), TypeCheckError> {
        check_types(&Module::new(vec![], vec![], vec![], vec![]))
    }

    #[test]
    fn check_variable_declaration() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
            vec![VariableDeclaration::new(
                "x",
                types::Primitive::PointerInteger,
            )],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                Block::new(
                    vec![
                        Load::new(types::Primitive::PointerInteger, Variable::new("x"), "y").into(),
                    ],
                    Return::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(42),
                    ),
                ),
                types::Primitive::PointerInteger,
                true,
            )],
        ))
    }

    #[test]
    fn check_variable_definition() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
            vec![],
            vec![],
            vec![VariableDefinition::new(
                "x",
                Primitive::PointerInteger(42),
                types::Primitive::PointerInteger,
                false,
                true,
            )],
            vec![FunctionDefinition::new(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                Block::new(
                    vec![
                        Load::new(types::Primitive::PointerInteger, Variable::new("x"), "y").into(),
                    ],
                    Return::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(42),
                    ),
                ),
                types::Primitive::PointerInteger,
                true,
            )],
        ))
    }

    #[test]
    fn check_function_declaration() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
            vec![],
            vec![FunctionDeclaration::new(
                "f",
                types::Function::new(
                    vec![types::Primitive::PointerInteger.into()],
                    types::Primitive::Float64,
                ),
            )],
            vec![],
            vec![FunctionDefinition::new(
                "g",
                vec![Argument::new("x", types::Primitive::PointerInteger)],
                Block::new(
                    vec![Call::new(
                        types::Function::new(
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
                types::Primitive::Float64,
                true,
            )],
        ))
    }

    #[test]
    fn check_return() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![],
                Block::new(
                    vec![],
                    Return::new(
                        types::Primitive::PointerInteger,
                        Primitive::PointerInteger(42),
                    ),
                ),
                types::Primitive::PointerInteger,
                true,
            )],
        ))
    }

    #[test]
    fn check_call() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![Argument::new("x", types::Primitive::PointerInteger)],
                Block::new(
                    vec![Call::new(
                        types::Function::new(
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
                types::Primitive::Float64,
                true,
            )],
        ))
    }

    #[test]
    fn check_if() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![],
                Block::new(
                    vec![If::new(
                        types::Primitive::Float64,
                        Primitive::Bool(true),
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
                types::Primitive::Float64,
                true,
            )],
        ))
    }

    #[test]
    fn check_load() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                Block::new(
                    vec![
                        Load::new(types::Primitive::PointerInteger, Variable::new("x"), "y").into(),
                    ],
                    Return::new(types::Primitive::PointerInteger, Variable::new("y")),
                ),
                types::Primitive::PointerInteger,
                true,
            )],
        ))
    }

    #[test]
    fn check_store() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
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
                types::Primitive::PointerInteger,
                true,
            )],
        ))
    }

    #[test]
    fn check_atomic_load() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
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
            )],
        ))
    }

    #[test]
    fn check_atomic_store() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                Block::new(
                    vec![AtomicStore::new(
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
                types::Primitive::PointerInteger,
                true,
            )],
        ))
    }

    #[test]
    fn check_pointer_address() -> Result<(), TypeCheckError> {
        let pointer_type = types::Pointer::new(types::Primitive::PointerInteger);

        check_types(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![Argument::new("x", pointer_type.clone())],
                Block::new(
                    vec![PointerAddress::new(
                        pointer_type.clone(),
                        Variable::new("x"),
                        Primitive::PointerInteger(42),
                        "y",
                    )
                    .into()],
                    Return::new(pointer_type.clone(), Variable::new("y")),
                ),
                pointer_type.clone(),
                true,
            )],
        ))
    }

    #[test]
    fn check_record_address() -> Result<(), TypeCheckError> {
        let record_type = types::Record::new(vec![types::Primitive::PointerInteger.into()]);

        check_types(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![Argument::new("x", types::Pointer::new(record_type.clone()))],
                Block::new(
                    vec![
                        RecordAddress::new(record_type.clone(), Variable::new("x"), 0, "y").into(),
                    ],
                    Return::new(
                        types::Pointer::new(types::Primitive::PointerInteger),
                        Variable::new("y"),
                    ),
                ),
                types::Pointer::new(types::Primitive::PointerInteger),
                true,
            )],
        ))
    }

    #[test]
    fn check_union_address() -> Result<(), TypeCheckError> {
        let union_type = types::Union::new(vec![types::Primitive::PointerInteger.into()]);

        check_types(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![Argument::new("x", types::Pointer::new(union_type.clone()))],
                Block::new(
                    vec![UnionAddress::new(union_type.clone(), Variable::new("x"), 0, "y").into()],
                    Return::new(
                        types::Pointer::new(types::Primitive::PointerInteger),
                        Variable::new("y"),
                    ),
                ),
                types::Pointer::new(types::Primitive::PointerInteger),
                true,
            )],
        ))
    }
}
