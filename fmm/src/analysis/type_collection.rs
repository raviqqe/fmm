use crate::{ir::*, types::Type};
use fnv::{FnvHashMap, FnvHashSet};

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

fn sort_types(types: &FnvHashSet<Type>) -> Vec<Type> {
    let mut graph = petgraph::graph::Graph::<&Type, ()>::new();
    let mut indices = FnvHashMap::<&Type, _>::new();

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

fn flat_types(types: &FnvHashSet<Type>) -> FnvHashSet<Type> {
    vec![]
        .into_iter()
        .chain(types.iter().flat_map(collect_from_type))
        .collect()
}

fn collect_from_expression(expression: &Expression) -> FnvHashSet<Type> {
    match expression {
        Expression::AlignOf(align_of) => [align_of.type_().clone()].into_iter().collect(),
        Expression::ArithmeticOperation(operation) => vec![operation.type_().into()]
            .into_iter()
            .chain(collect_from_expression(operation.lhs()))
            .chain(collect_from_expression(operation.rhs()))
            .collect(),
        Expression::BitCast(bit_cast) => vec![bit_cast.from().clone(), bit_cast.to().clone()]
            .into_iter()
            .chain(collect_from_expression(bit_cast.expression()))
            .collect(),
        Expression::BitwiseNotOperation(operation) => vec![operation.type_().into()]
            .into_iter()
            .chain(collect_from_expression(operation.value()))
            .collect(),
        Expression::BitwiseOperation(operation) => vec![operation.type_().into()]
            .into_iter()
            .chain(collect_from_expression(operation.lhs()))
            .chain(collect_from_expression(operation.rhs()))
            .collect(),
        Expression::ComparisonOperation(operation) => vec![operation.type_().into()]
            .into_iter()
            .chain(collect_from_expression(operation.lhs()))
            .chain(collect_from_expression(operation.rhs()))
            .collect(),
        Expression::PointerAddress(address) => vec![address.type_().clone().into()]
            .into_iter()
            .chain(collect_from_expression(address.pointer()))
            .collect(),
        Expression::Record(record) => vec![record.type_().clone().into()]
            .into_iter()
            .chain(record.fields().iter().flat_map(collect_from_expression))
            .collect(),
        Expression::RecordAddress(address) => vec![address.type_().clone().into()]
            .into_iter()
            .chain(collect_from_expression(address.pointer()))
            .collect(),
        Expression::SizeOf(size_of) => [size_of.type_().clone()].into_iter().collect(),
        Expression::Union(union) => vec![union.type_().clone().into()]
            .into_iter()
            .chain(collect_from_expression(union.member()))
            .collect(),
        Expression::UnionAddress(address) => vec![address.type_().clone().into()]
            .into_iter()
            .chain(collect_from_expression(address.pointer()))
            .collect(),
        Expression::Undefined(undefined) => [undefined.type_().clone()].into_iter().collect(),
        Expression::Primitive(_) | Expression::Variable(_) => Default::default(),
    }
}

fn collect_from_block(block: &Block) -> FnvHashSet<Type> {
    collect_from_instructions(block.instructions())
        .into_iter()
        .chain(collect_from_terminal_instruction(
            block.terminal_instruction(),
        ))
        .collect()
}

fn collect_from_instructions(instructions: &[Instruction]) -> FnvHashSet<Type> {
    let mut types = FnvHashSet::new();

    for instruction in instructions {
        types.extend(collect_from_instruction(instruction));
    }

    types
}

fn collect_from_instruction(instruction: &Instruction) -> FnvHashSet<Type> {
    match instruction {
        Instruction::AllocateHeap(allocate) => collect_from_expression(allocate.size()),
        Instruction::AllocateStack(allocate) => [allocate.type_().clone()].into_iter().collect(),
        Instruction::AtomicLoad(load) => [load.type_().clone()].into_iter().collect(),
        Instruction::AtomicOperation(operation) => vec![operation.type_().into()]
            .into_iter()
            .chain(collect_from_expression(operation.pointer()))
            .chain(collect_from_expression(operation.value()))
            .collect(),
        Instruction::AtomicStore(store) => [store.type_().clone()].into_iter().collect(),
        Instruction::Call(call) => vec![call.type_().clone().into()]
            .into_iter()
            .chain(collect_from_expression(call.function()))
            .chain(call.arguments().iter().flat_map(collect_from_expression))
            .collect(),
        Instruction::CompareAndSwap(cas) => [cas.type_().clone()].into_iter().collect(),
        Instruction::DeconstructRecord(deconstruct) => vec![deconstruct.type_().clone().into()]
            .into_iter()
            .chain(collect_from_expression(deconstruct.record()))
            .collect(),
        Instruction::DeconstructUnion(deconstruct) => vec![deconstruct.type_().clone().into()]
            .into_iter()
            .chain(collect_from_expression(deconstruct.union()))
            .collect(),
        Instruction::Fence(_) => Default::default(),
        Instruction::FreeHeap(free) => collect_from_expression(free.pointer()),
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
        Instruction::ReallocateHeap(reallocate) => vec![reallocate.pointer(), reallocate.size()]
            .into_iter()
            .flat_map(collect_from_expression)
            .collect(),
        Instruction::Store(store) => vec![store.type_().clone()]
            .into_iter()
            .chain(collect_from_expression(store.value()))
            .chain(collect_from_expression(store.pointer()))
            .collect(),
    }
}

fn collect_from_terminal_instruction(instruction: &TerminalInstruction) -> FnvHashSet<Type> {
    match instruction {
        TerminalInstruction::Branch(branch) => [branch.type_().clone()].into_iter().collect(),
        TerminalInstruction::Return(return_) => [return_.type_().clone()].into_iter().collect(),
        TerminalInstruction::Unreachable => Default::default(),
    }
}

fn collect_from_type(type_: &Type) -> FnvHashSet<Type> {
    vec![type_.clone()]
        .into_iter()
        .chain(collect_child_types(type_))
        .collect()
}

fn collect_child_types(type_: &Type) -> FnvHashSet<Type> {
    match type_ {
        Type::Function(function) => collect_from_type(function.result())
            .into_iter()
            .chain(function.arguments().iter().flat_map(collect_from_type))
            .collect(),
        Type::Primitive(_) => Default::default(),
        Type::Record(record) => record.fields().iter().flat_map(collect_from_type).collect(),
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
