mod error;

use crate::ir::*;
use crate::types::{self, Type};
pub use error::*;
use once_cell::sync::Lazy;
use std::collections::HashMap;

static GENERIC_POINTER_TYPE: Lazy<Type> =
    Lazy::new(|| types::Pointer::new(types::Primitive::Integer8).into());

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
            Instruction::AllocateHeap(_) | Instruction::AllocateStack(_) => {}
            Instruction::ArithmeticOperation(operation) => {
                check_equality(
                    &check_expression(operation.lhs(), &variables)?,
                    &operation.type_().into(),
                )?;
                check_equality(
                    &check_expression(operation.rhs(), &variables)?,
                    &operation.type_().into(),
                )?;
            }
            Instruction::AtomicLoad(load) => {
                check_equality(
                    &check_expression(load.pointer(), &variables)?,
                    &types::Pointer::new(load.type_().clone()).clone().into(),
                )?;
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

                check_equality(
                    &call.type_().clone().into(),
                    &check_expression(call.function(), &variables)?,
                )?;

                for (argument, type_) in call.arguments().iter().zip(call.type_().arguments()) {
                    check_equality(&check_expression(argument, &variables)?, type_)?;
                }
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
            }
            Instruction::DeconstructRecord(deconstruct) => {
                check_equality(
                    &check_expression(deconstruct.record(), &variables)?,
                    &deconstruct.type_().clone().into(),
                )?;

                if deconstruct.element_index() >= deconstruct.type_().elements().len() {
                    return Err(TypeCheckError::IndexOutOfRange);
                }
            }
            Instruction::DeconstructUnion(deconstruct) => {
                check_equality(
                    &check_expression(deconstruct.union(), &variables)?,
                    &deconstruct.type_().clone().into(),
                )?;

                if deconstruct.member_index() >= deconstruct.type_().members().len() {
                    return Err(TypeCheckError::IndexOutOfRange);
                }
            }
            Instruction::If(if_) => {
                check_equality(
                    &check_expression(if_.condition(), &variables)?,
                    &types::Primitive::Boolean.into(),
                )?;

                check_block(if_.then(), &variables, return_type, Some(if_.type_()))?;
                check_block(if_.else_(), &variables, return_type, Some(if_.type_()))?;
            }
            Instruction::Load(load) => {
                check_equality(
                    &check_expression(load.pointer(), &variables)?,
                    &types::Pointer::new(load.type_().clone()).into(),
                )?;
            }
            Instruction::PassThrough(pass) => {
                check_equality(
                    &check_expression(pass.expression(), &variables)?,
                    pass.type_(),
                )?;
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
            }
            Instruction::ReallocateHeap(reallocate) => {
                check_equality(
                    &check_expression(reallocate.pointer(), &variables)?,
                    &GENERIC_POINTER_TYPE,
                )?;

                check_equality(
                    &check_expression(reallocate.size(), &variables)?,
                    &types::Primitive::PointerInteger.into(),
                )?;
            }
            Instruction::RecordAddress(address) => {
                check_equality(
                    &check_expression(address.pointer(), &variables)?,
                    &types::Pointer::new(address.type_().clone()).into(),
                )?;

                if address.element_index() >= address.type_().elements().len() {
                    return Err(TypeCheckError::IndexOutOfRange);
                }
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

                if address.member_index() >= address.type_().members().len() {
                    return Err(TypeCheckError::IndexOutOfRange);
                }
            }
        }

        if let Some(name) = instruction.name() {
            if let Some(type_) = instruction.result_type() {
                variables.insert(name.into(), type_.clone());
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
        Expression::AlignOf(_) => AlignOf::RESULT_TYPE.into(),
        Expression::BitCast(bit_cast) => {
            check_equality(
                &check_expression(bit_cast.expression(), variables)?,
                bit_cast.from(),
            )?;

            bit_cast.to().clone()
        }
        Expression::Primitive(primitive) => primitive.type_().into(),
        Expression::SizeOf(_) => SizeOf::RESULT_TYPE.into(),
        Expression::Record(record) => {
            if record.elements().len() != record.type_().elements().len() {
                return Err(TypeCheckError::RecordElements(record.clone()));
            }

            for (element, type_) in record.elements().iter().zip(record.type_().elements()) {
                check_equality(&check_expression(element, variables)?, &type_)?;
            }

            record.type_().clone().into()
        }
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
        Expression::Variable(variable) => variables
            .get(variable.name())
            .cloned()
            .ok_or_else(|| TypeCheckError::VariableNotFound(variable.clone()))?,
    })
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
        types::Function::new(arguments, result, CallingConvention::Target)
    }

    fn create_function_definition(
        name: impl Into<String>,
        arguments: Vec<Argument>,
        body: Block,
        result_type: impl Into<Type>,
        global: bool,
    ) -> FunctionDefinition {
        FunctionDefinition::new(
            name,
            arguments,
            body,
            result_type,
            CallingConvention::Target,
            global,
        )
    }

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
            vec![create_function_definition(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                Block::new(
                    vec![
                        Load::new(types::Primitive::PointerInteger, Variable::new("x"), "y")
                            .into(),
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
            vec![create_function_definition(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                Block::new(
                    vec![
                        Load::new(types::Primitive::PointerInteger, Variable::new("x"), "y")
                            .into(),
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
                create_function_type(
                    vec![types::Primitive::PointerInteger.into()],
                    types::Primitive::Float64,
                ),
            )],
            vec![],
            vec![create_function_definition(
                "g",
                vec![Argument::new("x", types::Primitive::PointerInteger)],
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
            vec![create_function_definition(
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
    fn check_allocate_heap() -> Result<(), TypeCheckError> {
        let pointer_type = types::Pointer::new(types::Primitive::Float64);

        check_types(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![Argument::new("x", types::Primitive::PointerInteger)],
                Block::new(
                    vec![AllocateHeap::new(types::Primitive::Float64, "x").into()],
                    Return::new(pointer_type.clone(), Variable::new("x")),
                ),
                pointer_type,
                true,
            )],
        ))
    }

    #[test]
    fn check_reallocate_heap() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![Argument::new("x", GENERIC_POINTER_TYPE.clone())],
                Block::new(
                    vec![ReallocateHeap::new(
                        Variable::new("x"),
                        Primitive::PointerInteger(42),
                        "y",
                    )
                    .into()],
                    Return::new(GENERIC_POINTER_TYPE.clone(), Variable::new("y")),
                ),
                GENERIC_POINTER_TYPE.clone(),
                true,
            )],
        ))
    }

    #[test]
    fn check_allocate_stack() -> Result<(), TypeCheckError> {
        let pointer_type = types::Pointer::new(types::Primitive::Float64);

        check_types(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![Argument::new("x", types::Primitive::PointerInteger)],
                Block::new(
                    vec![AllocateStack::new(types::Primitive::Float64, "x").into()],
                    Return::new(pointer_type.clone(), Variable::new("x")),
                ),
                pointer_type,
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
            vec![create_function_definition(
                "f",
                vec![Argument::new("x", types::Primitive::PointerInteger)],
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
                types::Primitive::Float64,
                true,
            )],
        ))
    }

    #[test]
    #[should_panic]
    fn fail_to_check_call_with_wrong_function_type() {
        check_types(&Module::new(
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
                types::Primitive::Float64,
                true,
            )],
        ))
        .unwrap()
    }

    #[test]
    fn check_if() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![],
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
            vec![create_function_definition(
                "f",
                vec![Argument::new(
                    "x",
                    types::Pointer::new(types::Primitive::PointerInteger),
                )],
                Block::new(
                    vec![
                        Load::new(types::Primitive::PointerInteger, Variable::new("x"), "y")
                            .into(),
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
            vec![create_function_definition(
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
            vec![create_function_definition(
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
            vec![create_function_definition(
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
            vec![create_function_definition(
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
                pointer_type,
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
            vec![create_function_definition(
                "f",
                vec![Argument::new("x", types::Pointer::new(record_type.clone()))],
                Block::new(
                    vec![RecordAddress::new(record_type, Variable::new("x"), 0, "y").into()],
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
            vec![create_function_definition(
                "f",
                vec![Argument::new("x", types::Pointer::new(union_type.clone()))],
                Block::new(
                    vec![UnionAddress::new(union_type, Variable::new("x"), 0, "y").into()],
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
    fn check_align_of() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
            vec![],
            vec![],
            vec![VariableDefinition::new(
                "x",
                AlignOf::new(types::Primitive::Float64),
                types::Primitive::PointerInteger,
                false,
                true,
            )],
            vec![],
        ))
    }

    #[test]
    fn check_size_of() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
            vec![],
            vec![],
            vec![VariableDefinition::new(
                "x",
                SizeOf::new(types::Primitive::Float64),
                types::Primitive::PointerInteger,
                false,
                true,
            )],
            vec![],
        ))
    }

    #[test]
    fn check_bit_cast() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
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
                true,
            )],
            vec![],
        ))
    }

    #[test]
    fn check_pass_through() -> Result<(), TypeCheckError> {
        check_types(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
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
                true,
            )],
        ))
    }
}
