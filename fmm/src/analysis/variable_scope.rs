mod error;

use super::local_variable;
use crate::{
    ir::*,
    types::{self, generic_pointer_type, Type},
};
pub use error::*;
use fnv::FnvHashSet;

pub fn check(module: &Module) -> Result<(), VariableScopeError> {
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
        .collect::<FnvHashSet<_, _>>();

    for definition in module.variable_definitions() {
        check_variable_definition(definition, &variables)?;
    }

    for definition in module.function_definitions() {
        check_function_definition(definition, &mut variables)?;
    }

    Ok(())
}

fn check_variable_declarations(module: &Module) -> Result<(), VariableScopeError> {
    let variables = module
        .variable_declarations()
        .iter()
        .map(|declaration| (declaration.name(), declaration.type_().clone()))
        .collect::<FnvHashSet<_, _>>();

    for definition in module.variable_definitions() {
        if let Some(type_) = variables.get(definition.name()) {
            check_equality(definition.type_(), type_)?;
        }
    }

    Ok(())
}

fn check_function_declarations(module: &Module) -> Result<(), VariableScopeError> {
    let functions = module
        .function_declarations()
        .iter()
        .map(|declaration| (declaration.name(), declaration.type_().clone()))
        .collect::<FnvHashSet<_, _>>();

    for definition in module.function_definitions() {
        if let Some(type_) = functions.get(definition.name()) {
            check_equality(&definition.type_().clone().into(), &type_.clone().into())?;
        }
    }

    Ok(())
}

fn check_variable_definition(
    definition: &VariableDefinition,
    variables: &FnvHashSet<&str>,
) -> Result<(), VariableScopeError> {
    check_equality(
        &check_expression(definition.body(), variables)?,
        definition.type_(),
    )
}

fn check_function_definition<'a>(
    definition: &'a FunctionDefinition,
    variables: &mut FnvHashSet<&'a str>,
) -> Result<(), VariableScopeError> {
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
    return_type: &Type,
    branch_type: Option<&Type>,
    variables: &FnvHashSet<&str, Type>,
) -> Result<(), VariableScopeError> {
    for instruction in block.instructions() {
        match instruction {
            Instruction::AllocateHeap(allocate) => check_expression(allocate.size(), variables)?,
            Instruction::AllocateStack(_) => {}
            Instruction::AtomicLoad(load) => check_expression(load.pointer(), variables)?,
            Instruction::AtomicOperation(operation) => {
                check_expression(operation.pointer(), variables)?;
                check_expression(operation.value(), variables)?;
            }
            Instruction::AtomicStore(store) => {
                check_expression(store.value(), variables)?;
                check_expression(store.pointer(), variables)?;
            }
            Instruction::Call(call) => {
                check_expression(call.function(), variables)?;

                for (argument, type_) in call.arguments().iter().zip(call.type_().arguments()) {
                    check_expression(argument, variables)?;
                }
            }
            Instruction::CompareAndSwap(cas) => {
                check_expression(cas.pointer(), variables)?;
                check_expression(cas.old_value(), variables)?;
                check_expression(cas.new_value(), variables)?;
            }
            Instruction::DeconstructRecord(deconstruct) => {
                check_expression(deconstruct.record(), variables)?
            }
            Instruction::DeconstructUnion(deconstruct) => {
                check_expression(deconstruct.union(), variables)?
            }
            Instruction::Fence(_) => {}
            Instruction::FreeHeap(free) => check_expression(free.pointer(), variables)?,
            Instruction::If(if_) => {
                check_expression(if_.condition(), variables)?;

                check_block(if_.then(), return_type, Some(if_.type_()), variables)?;
                check_block(if_.else_(), return_type, Some(if_.type_()), variables)?;
            }
            Instruction::Load(load) => check_expression(load.pointer(), variables)?,
            Instruction::MemoryCopy(copy) => {
                check_expression(copy.source(), variables)?;
                check_expression(copy.destination(), variables)?;
                check_expression(copy.size(), variables)?;
            }
            Instruction::ReallocateHeap(reallocate) => {
                check_expression(reallocate.pointer(), variables)?;
                check_expression(reallocate.size(), variables)?;
            }
            Instruction::Store(store) => {
                check_expression(store.value(), variables)?;
                check_expression(store.pointer(), variables)?;
            }
        }
    }

    match block.terminal_instruction() {
        TerminalInstruction::Branch(branch) => check_expression(branch.expression(), variables)?,
        TerminalInstruction::Return(return_) => check_expression(return_.expression(), variables)?,
        TerminalInstruction::Unreachable => {}
    }

    Ok(())
}

fn check_expression(
    expression: &Expression,
    variables: &FnvHashSet<&str>,
) -> Result<(), VariableScopeError> {
    match expression {
        Expression::AlignOf(_) => {}
        Expression::ArithmeticOperation(operation) => {
            for expression in [operation.lhs(), operation.rhs()] {
                check_expression(expression, variables)?;
            }
        }
        Expression::BitCast(bit_cast) => check_expression(bit_cast.expression(), variables)?,
        Expression::BitwiseNotOperation(operation) => {
            check_expression(operation.value(), variables)?
        }
        Expression::BitwiseOperation(operation) => {
            for expression in [operation.lhs(), operation.rhs()] {
                check_expression(expression, variables)?;
            }
        }
        Expression::ComparisonOperation(operation) => {
            for expression in [operation.lhs(), operation.rhs()] {
                check_expression(expression, variables)?;
            }
        }
        Expression::PointerAddress(address) => {
            for expression in [address.pointer(), address.offset()] {
                check_expression(expression, variables)?;
            }
        }
        Expression::Primitive(primitive) => {}
        Expression::Record(record) => {
            for field in record.fields() {
                check_expression(field, variables)?;
            }
        }
        Expression::RecordAddress(address) => check_expression(address.pointer(), variables)?,
        Expression::SizeOf(_) => {}
        Expression::Undefined(undefined) => {}
        Expression::Union(union) => check_expression(union.member(), variables)?,
        Expression::UnionAddress(address) => check_expression(address.pointer(), variables)?,
        Expression::Variable(variable) => {
            if !variables.contains(variable.name()) {
                return Err(VariableScopeError::VariableNotFound(variable.clone()));
            }
        }
    }

    Ok(())
}
