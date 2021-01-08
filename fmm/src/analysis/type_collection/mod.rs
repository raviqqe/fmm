use crate::ir::*;
use crate::types::{self, Type};
use std::collections::HashSet;

pub fn collect_record_types(types: &HashSet<Type>) -> HashSet<types::Record> {
    types
        .iter()
        .filter_map(|type_| {
            if let Type::Record(record) = type_ {
                Some(record.clone())
            } else {
                None
            }
        })
        .collect()
}

pub fn collect_union_types(types: &HashSet<Type>) -> HashSet<types::Union> {
    types
        .iter()
        .filter_map(|type_| {
            if let Type::Union(union) = type_ {
                Some(union.clone())
            } else {
                None
            }
        })
        .collect()
}

pub fn collect_types(module: &Module) -> HashSet<Type> {
    flat_types(
        &module
            .variable_declarations()
            .iter()
            .map(|declaration| declaration.type_().clone())
            .chain(
                module
                    .function_declarations()
                    .iter()
                    .map(|declaration| declaration.type_().clone().into()),
            )
            .chain(module.variable_definitions().iter().flat_map(|definition| {
                Some(definition.type_().clone())
                    .into_iter()
                    .chain(collect_from_expression(definition.body()))
            }))
            .chain(module.variable_definitions().iter().flat_map(|definition| {
                Some(definition.type_().clone())
                    .into_iter()
                    .chain(collect_from_expression(definition.body()))
            }))
            .chain(module.function_definitions().iter().flat_map(|definition| {
                Some(definition.type_().clone().into())
                    .into_iter()
                    .chain(collect_from_block(definition.body()))
            }))
            .collect(),
    )
}

fn flat_types(types: &HashSet<Type>) -> HashSet<Type> {
    types
        .iter()
        .cloned()
        .chain(types.iter().flat_map(collect_from_type))
        .collect()
}

fn collect_from_expression(expression: &Expression) -> HashSet<Type> {
    match expression {
        Expression::Record(record) => vec![record.type_().clone().into()]
            .into_iter()
            .chain(record.elements().iter().flat_map(collect_from_expression))
            .collect(),
        Expression::Union(union) => vec![union.type_().clone().into()]
            .into_iter()
            .chain(collect_from_expression(union.member()))
            .collect(),
        Expression::Undefined(undefined) => vec![undefined.type_().clone()].into_iter().collect(),
        Expression::Primitive(_) | Expression::Variable(_) => Default::default(),
    }
}

fn collect_from_block(block: &Block) -> HashSet<Type> {
    collect_from_instructions(block.instructions())
        .into_iter()
        .chain(collect_from_terminal_instruction(
            block.terminal_instruction(),
        ))
        .collect()
}

fn collect_from_instructions(instructions: &[Instruction]) -> HashSet<Type> {
    let mut types = HashSet::new();

    for instruction in instructions {
        types.extend(collect_from_instruction(instruction));
    }

    types
}

fn collect_from_instruction(instruction: &Instruction) -> HashSet<Type> {
    match instruction {
        Instruction::AllocateHeap(allocate) => vec![allocate.type_().clone()].into_iter().collect(),
        Instruction::ArithmeticOperation(_) => Default::default(),
        Instruction::Assignment(assignment) => vec![assignment.type_().clone()]
            .into_iter()
            .chain(collect_from_expression(assignment.expression()))
            .collect(),
        Instruction::AtomicLoad(load) => vec![load.type_().clone()].into_iter().collect(),
        Instruction::AtomicStore(store) => vec![store.type_().clone()].into_iter().collect(),
        Instruction::Call(call) => vec![call.type_().clone().into()]
            .into_iter()
            .chain(collect_from_expression(call.function()))
            .chain(call.arguments().iter().flat_map(collect_from_expression))
            .collect(),
        Instruction::CompareAndSwap(cas) => vec![cas.type_().clone()].into_iter().collect(),
        Instruction::ComparisonOperation(_) => Default::default(),
        Instruction::DeconstructRecord(deconstruct) => vec![deconstruct.type_().clone().into()]
            .into_iter()
            .chain(collect_from_expression(deconstruct.record()))
            .collect(),
        Instruction::DeconstructUnion(deconstruct) => vec![deconstruct.type_().clone().into()]
            .into_iter()
            .chain(collect_from_expression(deconstruct.union()))
            .collect(),
        Instruction::If(if_) => vec![if_.type_().clone()]
            .into_iter()
            .chain(collect_from_expression(if_.condition()))
            .chain(collect_from_block(if_.then()))
            .chain(collect_from_block(if_.else_()))
            .collect(),
        Instruction::Load(load) => vec![load.type_().clone()]
            .into_iter()
            .chain(collect_from_expression(load.pointer()))
            .collect(),
        Instruction::PointerAddress(address) => vec![address.type_().clone().into()]
            .into_iter()
            .chain(collect_from_expression(address.pointer()))
            .collect(),
        Instruction::RecordAddress(address) => vec![address.type_().clone().into()]
            .into_iter()
            .chain(collect_from_expression(address.pointer()))
            .collect(),
        Instruction::Store(store) => vec![store.type_().clone()]
            .into_iter()
            .chain(collect_from_expression(store.value()))
            .chain(collect_from_expression(store.pointer()))
            .collect(),
        Instruction::UnionAddress(address) => vec![address.type_().clone().into()]
            .into_iter()
            .chain(collect_from_expression(address.pointer()))
            .collect(),
    }
}

fn collect_from_terminal_instruction(instruction: &TerminalInstruction) -> HashSet<Type> {
    match instruction {
        TerminalInstruction::Branch(branch) => vec![branch.type_().clone()].into_iter().collect(),
        TerminalInstruction::Return(return_) => vec![return_.type_().clone()].into_iter().collect(),
        TerminalInstruction::Unreachable => Default::default(),
    }
}

fn collect_from_type(type_: &Type) -> HashSet<Type> {
    match type_ {
        Type::Function(function) => vec![function.result().clone()]
            .into_iter()
            .chain(function.arguments().iter().flat_map(collect_from_type))
            .collect(),
        Type::Primitive(_) => Default::default(),
        Type::Record(record) => record
            .elements()
            .iter()
            .flat_map(collect_from_type)
            .collect(),
        Type::Pointer(pointer) => collect_from_type(pointer.element()),
        Type::Union(union) => union.members().iter().flat_map(collect_from_type).collect(),
    }
}
