use crate::ir::*;
use indexmap::IndexSet;

pub fn collect(instructions: &mut [Instruction], terminal_instruction: &TerminalInstruction) {
    let mut variables = IndexSet::default();

    collect_from_instructions(instructions, terminal_instruction, &mut variables);
}

fn collect_from_instructions(
    instructions: &mut [Instruction],
    terminal_instruction: &TerminalInstruction,
    variables: &mut IndexSet<String>,
) {
    collect_from_terminal_instruction(terminal_instruction, variables);

    for instruction in instructions.iter_mut().rev() {
        if let Some((name, _)) = instruction.value() {
            variables.remove(name);
        }

        collect_from_instruction(instruction, variables);
    }
}

fn collect_from_block(block: &mut Block, variables: &mut IndexSet<String>) {
    // TODO Avoid this clone.
    let terminal_instruction = block.terminal_instruction().clone();

    collect_from_instructions(block.instructions_mut(), &terminal_instruction, variables);
}

fn collect_from_instruction(instruction: &mut Instruction, variables: &mut IndexSet<String>) {
    let mut collect_from_expression = |expression| collect_from_expression(expression, variables);

    match instruction {
        Instruction::AllocateHeap(allocate) => collect_from_expression(allocate.size()),
        Instruction::AtomicLoad(load) => collect_from_expression(load.pointer()),
        Instruction::AtomicOperation(operation) => {
            collect_from_expression(operation.pointer());
            collect_from_expression(operation.value());
        }
        Instruction::AtomicStore(store) => {
            collect_from_expression(store.value());
            collect_from_expression(store.pointer());
        }
        Instruction::Call(call) => {
            collect_from_expression(call.function());

            for argument in call.arguments() {
                collect_from_expression(argument);
            }
        }
        Instruction::CompareAndSwap(cas) => {
            collect_from_expression(cas.pointer());
            collect_from_expression(cas.old_value());
            collect_from_expression(cas.new_value());
        }
        Instruction::DeconstructRecord(deconstruct) => {
            collect_from_expression(deconstruct.record())
        }
        Instruction::DeconstructUnion(deconstruct) => collect_from_expression(deconstruct.union()),
        Instruction::FreeHeap(free) => collect_from_expression(free.pointer()),
        Instruction::If(if_) => {
            collect_from_expression(if_.condition());
            collect_from_block(if_.then_mut(), variables);
            collect_from_block(if_.else_mut(), variables);
        }
        Instruction::Load(load) => collect_from_expression(load.pointer()),
        Instruction::MemoryCopy(copy) => {
            collect_from_expression(copy.source());
            collect_from_expression(copy.destination());
            collect_from_expression(copy.size());
        }
        Instruction::ReallocateHeap(reallocate) => {
            collect_from_expression(reallocate.pointer());
            collect_from_expression(reallocate.size());
        }
        Instruction::Store(store) => {
            collect_from_expression(store.value());
            collect_from_expression(store.pointer());
        }
        Instruction::Fence(_) | Instruction::AllocateStack(_) => Default::default(),
    }
}

fn collect_from_terminal_instruction(
    instruction: &TerminalInstruction,
    variables: &mut IndexSet<String>,
) {
    let mut collect_from_expression = |expression| collect_from_expression(expression, variables);

    match instruction {
        TerminalInstruction::Branch(branch) => collect_from_expression(branch.expression()),
        TerminalInstruction::Return(return_) => collect_from_expression(return_.expression()),
        TerminalInstruction::Unreachable => {}
    }
}

fn collect_from_expression(expression: &Expression, variables: &mut IndexSet<String>) {
    let mut collect_from_expression = |expression| collect_from_expression(expression, variables);

    match expression {
        Expression::ArithmeticOperation(operation) => {
            collect_from_expression(operation.lhs());
            collect_from_expression(operation.rhs());
        }
        Expression::BitCast(bit_cast) => collect_from_expression(bit_cast.expression()),
        Expression::BitwiseNotOperation(operation) => collect_from_expression(operation.value()),
        Expression::BitwiseOperation(operation) => {
            collect_from_expression(operation.lhs());
            collect_from_expression(operation.rhs());
        }
        Expression::ComparisonOperation(operation) => {
            collect_from_expression(operation.lhs());
            collect_from_expression(operation.rhs());
        }
        Expression::PointerAddress(address) => {
            collect_from_expression(address.pointer());
            collect_from_expression(address.offset());
        }
        Expression::Record(record) => {
            for field in record.fields() {
                collect_from_expression(field);
            }
        }
        Expression::RecordAddress(address) => collect_from_expression(address.pointer()),
        Expression::Union(union) => collect_from_expression(union.member()),
        Expression::UnionAddress(address) => collect_from_expression(address.pointer()),
        Expression::Variable(variable) => {
            variables.insert(variable.name().into());
        }
        Expression::AlignOf(_)
        | Expression::Primitive(_)
        | Expression::SizeOf(_)
        | Expression::Undefined(_) => {}
    }
}
