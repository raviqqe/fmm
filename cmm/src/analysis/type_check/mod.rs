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
    check_instructions(
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
    )?;

    Ok(())
}

fn check_instructions(
    instructions: &[Instruction],
    variables: &HashMap<String, Type>,
    return_type: &Type,
) -> Result<HashMap<String, Type>, TypeCheckError> {
    let mut variables = variables.clone();

    for instruction in instructions {
        match instruction {
            Instruction::AllocateHeap(allocate) => {
                variables.insert(
                    allocate.name().into(),
                    types::Pointer::new(allocate.type_().clone()).into(),
                );
            }
            Instruction::AllocateStack(allocate) => {
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
            Instruction::Assignment(assignment) => {
                check_equality(
                    &check_expression(assignment.expression(), &variables)?,
                    &assignment.type_(),
                )?;

                variables.insert(assignment.name().into(), assignment.type_().clone());
            }
            Instruction::AtomicLoad(load) => {
                check_equality(
                    &check_expression(load.pointer(), &variables)?,
                    &types::Pointer::new(load.type_()).into(),
                )?;

                variables.insert(load.name().into(), load.type_().clone().into());
            }
            Instruction::AtomicStore(store) => {
                check_equality(
                    &check_expression(store.value(), &variables)?,
                    &store.type_().into(),
                )?;
                check_equality(
                    &check_expression(store.pointer(), &variables)?,
                    &types::Pointer::new(store.type_()).into(),
                )?;
            }
            Instruction::Bitcast(bitcast) => {
                check_equality(
                    &check_expression(bitcast.expression(), &variables)?,
                    &types::Pointer::new(bitcast.from_type()).into(),
                )?;

                variables.insert(bitcast.name().into(), bitcast.to_type().into());
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
                    &types::Pointer::new(cas.type_()).into(),
                )?;

                check_equality(
                    &check_expression(cas.old_value(), &variables)?,
                    &cas.type_().into(),
                )?;

                check_equality(
                    &check_expression(cas.new_value(), &variables)?,
                    &cas.type_().into(),
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
                    address
                        .type_()
                        .elements()
                        .get(address.element_index())
                        .ok_or(TypeCheckError::IndexOutOfRange)?
                        .clone(),
                );
            }
            Instruction::Return(return_) => {
                check_equality(
                    &check_expression(return_.expression(), &variables)?,
                    return_.type_(),
                )?;
                check_equality(return_.type_(), return_type)?;
            }
            Instruction::Store(store) => {
                check_equality(&check_expression(store.value(), &variables)?, store.type_())?;
                check_equality(
                    &check_expression(store.pointer(), &variables)?,
                    &types::Pointer::new(store.type_().clone()).into(),
                )?;
            }
            Instruction::Switch(switch) => {
                check_equality(
                    &check_expression(switch.condition(), &variables)?,
                    switch.condition_type(),
                )?;

                for alternative in switch.alternatives() {
                    check_equality(
                        &check_expression(alternative.condition(), &variables)?,
                        switch.condition_type(),
                    )?;

                    let variables =
                        check_instructions(alternative.instructions(), &variables, return_type)?;

                    check_equality(
                        &check_expression(alternative.result(), &variables)?,
                        switch.result_type(),
                    )?;
                }

                variables.insert(switch.name().into(), switch.result_type().clone());
            }
            Instruction::UnionAddress(address) => {
                check_equality(
                    &check_expression(address.pointer(), &variables)?,
                    &types::Pointer::new(address.type_().clone()).into(),
                )?;

                variables.insert(
                    address.name().into(),
                    address
                        .type_()
                        .members()
                        .get(address.member_index())
                        .ok_or(TypeCheckError::IndexOutOfRange)?
                        .clone(),
                );
            }
            Instruction::Unreachable => {}
        }
    }

    Ok(variables)
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
    fn check_return() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![],
                vec![Return::new(
                    types::Primitive::PointerInteger,
                    Primitive::PointerInteger(42),
                )
                .into()],
                types::Primitive::PointerInteger,
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
                vec![
                    Call::new(
                        types::Function::new(
                            vec![types::Primitive::PointerInteger.into()],
                            types::Primitive::Float64,
                        ),
                        Variable::new("f"),
                        vec![Primitive::PointerInteger(42).into()],
                        "x",
                    )
                    .into(),
                    Return::new(types::Primitive::Float64, Variable::new("x")).into(),
                ],
                types::Primitive::Float64,
            )],
        ))
    }

    #[test]
    fn check_switch() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![],
                vec![
                    Switch::new(
                        types::Primitive::PointerInteger,
                        types::Primitive::Float64,
                        Primitive::PointerInteger(42),
                        vec![Alternative::new(
                            Primitive::PointerInteger(42),
                            vec![],
                            Primitive::Float64(42.0),
                        )],
                        DefaultAlternative::new(vec![], Primitive::Float64(42.0)),
                        "x",
                    )
                    .into(),
                    Return::new(types::Primitive::Float64, Variable::new("x")).into(),
                ],
                types::Primitive::Float64,
            )],
        ))
    }
}
