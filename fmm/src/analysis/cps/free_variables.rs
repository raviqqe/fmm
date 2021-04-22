use crate::ir::*;
use std::collections::HashSet;

pub fn collect_free_variables(
    instructions: &[Instruction],
    terminal_instruction: &TerminalInstruction,
) -> HashSet<String> {
    let mut variables = HashSet::new();

    for instruction in instructions {
        variables.extend(collect_from_instruction(instruction));

        if let Some(name) = instruction.name() {
            variables.remove(name);
        }
    }

    variables.extend(collect_from_terminal_instruction(terminal_instruction));

    variables
}

fn collect_from_block(block: &Block) -> HashSet<String> {
    collect_free_variables(block.instructions(), block.terminal_instruction())
}

fn collect_from_instruction(instruction: &Instruction) -> HashSet<String> {
    match instruction {
        Instruction::AtomicLoad(load) => collect_from_expression(load.pointer()),
        Instruction::AtomicOperation(operation) => [operation.pointer(), operation.value()]
            .iter()
            .flat_map(|expression| collect_from_expression(expression))
            .collect(),
        Instruction::AtomicStore(store) => [store.value(), store.pointer()]
            .iter()
            .flat_map(|expression| collect_from_expression(expression))
            .collect(),
        Instruction::Call(call) => collect_from_expression(call.function())
            .into_iter()
            .chain(call.arguments().iter().flat_map(collect_from_expression))
            .collect(),
        Instruction::CompareAndSwap(cas) => [cas.pointer(), cas.old_value(), cas.new_value()]
            .iter()
            .flat_map(|expression| collect_from_expression(*expression))
            .collect(),
        Instruction::DeconstructRecord(deconstruct) => {
            collect_from_expression(deconstruct.record())
        }
        Instruction::DeconstructUnion(deconstruct) => collect_from_expression(deconstruct.union()),
        Instruction::FreeHeap(free) => collect_from_expression(free.pointer()),
        Instruction::If(if_) => vec![
            collect_from_expression(if_.condition()),
            collect_from_block(if_.then()),
            collect_from_block(if_.else_()),
        ]
        .into_iter()
        .flatten()
        .collect(),
        Instruction::Load(load) => collect_from_expression(load.pointer()),
        Instruction::PassThrough(pass) => collect_from_expression(pass.expression()),
        Instruction::PointerAddress(address) => [address.pointer(), address.offset()]
            .iter()
            .flat_map(|expression| collect_from_expression(*expression))
            .collect(),
        Instruction::ReallocateHeap(reallocate) => [reallocate.pointer(), reallocate.size()]
            .iter()
            .flat_map(|expression| collect_from_expression(*expression))
            .collect(),
        Instruction::RecordAddress(address) => collect_from_expression(address.pointer()),
        Instruction::Store(store) => [store.value(), store.pointer()]
            .iter()
            .flat_map(|expression| collect_from_expression(*expression))
            .collect(),
        Instruction::UnionAddress(address) => collect_from_expression(address.pointer()),

        Instruction::AllocateHeap(_) | Instruction::AllocateStack(_) => Default::default(),
    }
}

fn collect_from_terminal_instruction(instruction: &TerminalInstruction) -> HashSet<String> {
    match instruction {
        TerminalInstruction::Branch(branch) => collect_from_expression(branch.expression()),
        TerminalInstruction::Return(return_) => collect_from_expression(return_.expression()),
        TerminalInstruction::Unreachable => HashSet::new(),
    }
}

fn collect_from_expression(expression: &Expression) -> HashSet<String> {
    match expression {
        Expression::ArithmeticOperation(operation) => [operation.lhs(), operation.rhs()]
            .iter()
            .flat_map(|expression| collect_from_expression(*expression))
            .collect(),
        Expression::BitCast(bit_cast) => collect_from_expression(bit_cast.expression()),
        Expression::BitwiseNotOperation(operation) => collect_from_expression(operation.value()),
        Expression::BitwiseOperation(operation) => collect_from_expression(operation.lhs())
            .into_iter()
            .chain(collect_from_expression(operation.rhs()))
            .collect(),
        Expression::ComparisonOperation(operation) => [operation.lhs(), operation.rhs()]
            .iter()
            .flat_map(|expression| collect_from_expression(*expression))
            .collect(),
        Expression::Record(record) => record
            .elements()
            .iter()
            .flat_map(collect_from_expression)
            .collect(),
        Expression::Union(union) => collect_from_expression(union.member()),
        Expression::Variable(variable) => vec![variable.name().into()].into_iter().collect(),
        Expression::AlignOf(_)
        | Expression::Primitive(_)
        | Expression::SizeOf(_)
        | Expression::Undefined(_) => Default::default(),
    }
}
