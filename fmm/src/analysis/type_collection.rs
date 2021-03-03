use crate::ir::*;
use crate::types::Type;
use std::collections::{HashMap, HashSet};

pub fn collect_types(module: &Module) -> Vec<Type> {
    sort_types(&flat_types(
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
            .chain(module.function_definitions().iter().flat_map(|definition| {
                Some(definition.type_().clone().into())
                    .into_iter()
                    .chain(collect_from_block(definition.body()))
            }))
            .collect(),
    ))
}

fn sort_types(types: &HashSet<Type>) -> Vec<Type> {
    let mut graph = petgraph::graph::Graph::<&Type, ()>::new();
    let mut indices = HashMap::<&Type, _>::new();

    for type_ in types {
        indices.insert(type_, graph.add_node(type_));
    }

    for type_ in types {
        for child_type in collect_child_types(type_) {
            graph.add_edge(indices[&child_type], indices[type_], ());
        }
    }

    petgraph::algo::toposort(&graph, None)
        .unwrap()
        .into_iter()
        .map(|index| graph[index].clone())
        .collect()
}

fn flat_types(types: &HashSet<Type>) -> HashSet<Type> {
    vec![]
        .into_iter()
        .chain(types.iter().flat_map(collect_from_type))
        .collect()
}

fn collect_from_expression(expression: &Expression) -> HashSet<Type> {
    match expression {
        Expression::AlignOf(align_of) => vec![align_of.type_().clone()].into_iter().collect(),
        Expression::BitCast(bit_cast) => vec![bit_cast.from().clone(), bit_cast.to().clone()]
            .into_iter()
            .chain(collect_from_expression(bit_cast.expression()))
            .collect(),
        Expression::Record(record) => vec![record.type_().clone().into()]
            .into_iter()
            .chain(record.elements().iter().flat_map(collect_from_expression))
            .collect(),
        Expression::SizeOf(size_of) => vec![size_of.type_().clone()].into_iter().collect(),
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
        Instruction::AllocateStack(allocate) => {
            vec![allocate.type_().clone()].into_iter().collect()
        }
        Instruction::ArithmeticOperation(_) => Default::default(),
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
        Instruction::PassThrough(pass) => vec![pass.type_().clone()]
            .into_iter()
            .chain(collect_from_expression(pass.expression()))
            .collect(),
        Instruction::PointerAddress(address) => vec![address.type_().clone().into()]
            .into_iter()
            .chain(collect_from_expression(address.pointer()))
            .collect(),
        Instruction::ReallocateHeap(reallocate) => vec![reallocate.pointer(), reallocate.size()]
            .into_iter()
            .flat_map(collect_from_expression)
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
    vec![type_.clone()]
        .into_iter()
        .chain(collect_child_types(type_))
        .collect()
}

fn collect_child_types(type_: &Type) -> HashSet<Type> {
    match type_ {
        Type::Function(function) => collect_from_type(function.result())
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{self, CallingConvention};

    #[test]
    fn sort_types() {
        assert_eq!(
            collect_types(&Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Record::new(vec![types::Record::new(vec![]).into()])
                )],
                vec![],
                vec![],
                vec![]
            )),
            vec![
                types::Record::new(vec![]).into(),
                types::Record::new(vec![types::Record::new(vec![]).into()]).into()
            ]
        );
    }

    #[test]
    fn collect_from_nested_function_types() {
        assert_eq!(
            collect_types(&Module::new(
                vec![],
                vec![FunctionDeclaration::new(
                    "x",
                    types::Function::new(
                        vec![],
                        types::Function::new(
                            vec![],
                            types::Primitive::PointerInteger,
                            CallingConvention::Target
                        ),
                        CallingConvention::Target
                    )
                )],
                vec![],
                vec![]
            )),
            vec![
                types::Primitive::PointerInteger.into(),
                types::Function::new(
                    vec![],
                    types::Primitive::PointerInteger,
                    CallingConvention::Target
                )
                .into(),
                types::Function::new(
                    vec![],
                    types::Function::new(
                        vec![],
                        types::Primitive::PointerInteger,
                        CallingConvention::Target,
                    ),
                    CallingConvention::Target
                )
                .into(),
            ]
        );
    }
}
