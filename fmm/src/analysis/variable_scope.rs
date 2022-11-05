mod error;

use crate::ir::*;
pub use error::*;
use fnv::FnvHashSet;

pub fn check(module: &Module) -> Result<(), VariableScopeError> {
    let mut variables = module
        .variable_declarations()
        .iter()
        .map(|declaration| declaration.name())
        .chain(
            module
                .function_declarations()
                .iter()
                .map(|declaration| declaration.name()),
        )
        .chain(
            module
                .variable_definitions()
                .iter()
                .map(|definition| definition.name()),
        )
        .chain(
            module
                .function_definitions()
                .iter()
                .map(|definition| definition.name()),
        )
        .collect::<FnvHashSet<_>>();

    for definition in module.variable_definitions() {
        check_variable_definition(definition, &variables)?;
    }

    for definition in module.function_definitions() {
        check_function_definition(definition, &mut variables)?;
    }

    Ok(())
}

fn check_variable_definition(
    definition: &VariableDefinition,
    variables: &FnvHashSet<&str>,
) -> Result<(), VariableScopeError> {
    check_expression(definition.body(), variables)
}

fn check_function_definition<'a>(
    definition: &'a FunctionDefinition,
    variables: &mut FnvHashSet<&'a str>,
) -> Result<(), VariableScopeError> {
    for argument in definition.arguments() {
        variables.insert(argument.name());
    }

    check_block(definition.body(), definition.result_type(), variables)?;

    for argument in definition.arguments() {
        variables.remove(argument.name());
    }

    Ok(())
}

fn check_block<'a>(
    block: &'a Block,
    return_type: &Type,
    variables: &mut FnvHashSet<&'a str>,
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

                for argument in call.arguments() {
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

                check_block(if_.then(), return_type, variables)?;
                check_block(if_.else_(), return_type, variables)?;
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

        for instruction in block.instructions() {
            if let Some((name, _)) = instruction.value() {
                variables.insert(name);
            }
        }
    }

    match block.terminal_instruction() {
        TerminalInstruction::Branch(branch) => check_expression(branch.expression(), variables)?,
        TerminalInstruction::Return(return_) => check_expression(return_.expression(), variables)?,
        TerminalInstruction::Unreachable => {}
    }

    for instruction in block.instructions() {
        if let Some((name, _)) = instruction.value() {
            variables.remove(name);
        }
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
        Expression::Primitive(_) => {}
        Expression::Record(record) => {
            for field in record.fields() {
                check_expression(field, variables)?;
            }
        }
        Expression::RecordAddress(address) => check_expression(address.pointer(), variables)?,
        Expression::SizeOf(_) => {}
        Expression::Undefined(_) => {}
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
