use crate::ir::*;
use fnv::FnvHashSet;

pub fn collect<'a>(
    instructions: &'a [Instruction],
    terminal_instruction: &'a TerminalInstruction,
) -> FnvHashSet<&'a str> {
    let mut variables = FnvHashSet::default();

    collect_from_instructions(instructions, terminal_instruction, &mut variables);

    variables
}

fn collect_from_instructions<'a>(
    instructions: &'a [Instruction],
    terminal_instruction: &'a TerminalInstruction,
    variables: &mut FnvHashSet<&'a str>,
) {
    for instruction in instructions {
        collect_from_instruction(instruction, variables);

        if let Some((name, _)) = instruction.value() {
            variables.remove(name);
        }
    }

    collect_from_terminal_instruction(terminal_instruction, variables);
}

fn collect_from_block<'a>(block: &'a Block, variables: &mut FnvHashSet<&'a str>) {
    collect_from_instructions(
        block.instructions(),
        block.terminal_instruction(),
        variables,
    );
}

fn collect_from_instruction<'a>(instruction: &'a Instruction, variables: &mut FnvHashSet<&'a str>) {
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
            collect_from_block(if_.then(), variables);
            collect_from_block(if_.else_(), variables);
        }
        Instruction::Load(load) => collect_from_expression(load.pointer()),
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

fn collect_from_terminal_instruction<'a>(
    instruction: &'a TerminalInstruction,
    variables: &mut FnvHashSet<&'a str>,
) {
    let mut collect_from_expression = |expression| collect_from_expression(expression, variables);

    match instruction {
        TerminalInstruction::Branch(branch) => collect_from_expression(branch.expression()),
        TerminalInstruction::Return(return_) => collect_from_expression(return_.expression()),
        TerminalInstruction::Unreachable => {}
    }
}

fn collect_from_expression<'a>(expression: &'a Expression, variables: &mut FnvHashSet<&'a str>) {
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
            variables.insert(variable.name());
        }
        Expression::AlignOf(_)
        | Expression::Primitive(_)
        | Expression::SizeOf(_)
        | Expression::Undefined(_) => {}
    }
}
