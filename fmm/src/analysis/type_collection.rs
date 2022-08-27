use crate::{ir::*, types::Type};
use fnv::{FnvHashMap, FnvHashSet};

pub fn collect_types(module: &Module) -> Vec<Type> {
    let mut types = FnvHashSet::default();

    for declaration in module.variable_declarations() {
        types.insert(declaration.type_().clone());
    }

    for declaration in module.function_declarations() {
        types.insert(declaration.type_().clone().into());
    }

    for definition in module.variable_definitions() {
        types.insert(definition.type_().clone());
        collect_from_expression(definition.body(), &mut types);
    }

    for definition in module.function_definitions() {
        types.insert(definition.type_().clone().into());
        collect_from_block(definition.body(), &mut types);
    }

    let mut all_types = FnvHashSet::default();

    for type_ in types {
        collect_from_type(&type_, &mut all_types);
    }

    sort_types(&all_types)
}

fn sort_types(types: &FnvHashSet<Type>) -> Vec<Type> {
    let mut graph = petgraph::graph::Graph::<&Type, ()>::new();
    let mut indices = FnvHashMap::<&Type, _>::default();

    for type_ in types {
        indices.insert(type_, graph.add_node(type_));
    }

    for type_ in types {
        let mut children = FnvHashSet::default();

        collect_child_types(type_, &mut children);

        for child_type in children {
            graph.add_edge(indices[&child_type], indices[type_], ());
        }
    }

    petgraph::algo::toposort(&graph, None)
        .unwrap()
        .into_iter()
        .map(|index| graph[index].clone())
        .collect()
}

fn collect_from_expression(expression: &Expression, types: &mut FnvHashSet<Type>) {
    let mut collect_from_expression = |expression| collect_from_expression(expression, types);

    match expression {
        Expression::AlignOf(align_of) => {
            types.insert(align_of.type_().clone());
        }
        Expression::ArithmeticOperation(operation) => {
            collect_from_expression(operation.lhs());
            collect_from_expression(operation.rhs());

            types.insert(operation.type_().into());
        }
        Expression::BitCast(bit_cast) => {
            collect_from_expression(bit_cast.expression());

            types.insert(bit_cast.from().clone());
            types.insert(bit_cast.to().clone());
        }
        Expression::BitwiseNotOperation(operation) => {
            collect_from_expression(operation.value());

            types.insert(operation.type_().into());
        }
        Expression::BitwiseOperation(operation) => {
            collect_from_expression(operation.lhs());
            collect_from_expression(operation.rhs());

            types.insert(operation.type_().into());
        }
        Expression::ComparisonOperation(operation) => {
            collect_from_expression(operation.lhs());
            collect_from_expression(operation.rhs());

            types.insert(operation.type_().into());
        }
        Expression::PointerAddress(address) => {
            collect_from_expression(address.pointer());

            types.insert(address.type_().clone().into());
        }
        Expression::Record(record) => {
            for field in record.fields() {
                collect_from_expression(field)
            }

            types.insert(record.type_().clone().into());
        }
        Expression::RecordAddress(address) => {
            collect_from_expression(address.pointer());

            types.insert(address.type_().clone().into());
        }
        Expression::SizeOf(size_of) => {
            types.insert(size_of.type_().clone());
        }
        Expression::Union(union) => {
            collect_from_expression(union.member());

            types.insert(union.type_().clone().into());
        }
        Expression::UnionAddress(address) => {
            collect_from_expression(address.pointer());

            types.insert(address.type_().clone().into());
        }
        Expression::Undefined(undefined) => {
            types.insert(undefined.type_().clone());
        }
        Expression::Primitive(_) | Expression::Variable(_) => {}
    }
}

fn collect_from_block(block: &Block, types: &mut FnvHashSet<Type>) {
    collect_from_instructions(block.instructions(), types);
    collect_from_terminal_instruction(block.terminal_instruction(), types);
}

fn collect_from_instructions(instructions: &[Instruction], types: &mut FnvHashSet<Type>) {
    for instruction in instructions {
        collect_from_instruction(instruction, types);
    }
}

fn collect_from_instruction(instruction: &Instruction, types: &mut FnvHashSet<Type>) {
    let mut collect_from_expression = |expression| collect_from_expression(expression, types);

    match instruction {
        Instruction::AllocateHeap(allocate) => collect_from_expression(allocate.size()),
        Instruction::AllocateStack(allocate) => {
            types.insert(allocate.type_().clone());
        }
        Instruction::AtomicLoad(load) => {
            collect_from_expression(load.pointer());

            types.insert(load.type_().clone());
        }
        Instruction::AtomicOperation(operation) => {
            collect_from_expression(operation.pointer());
            collect_from_expression(operation.value());

            types.insert(operation.type_().into());
        }
        Instruction::AtomicStore(store) => {
            collect_from_expression(store.value());
            collect_from_expression(store.pointer());

            types.insert(store.type_().clone());
        }
        Instruction::Call(call) => {
            collect_from_expression(call.function());

            for argument in call.arguments() {
                collect_from_expression(argument);
            }

            types.insert(call.type_().clone().into());
        }
        Instruction::CompareAndSwap(cas) => {
            collect_from_expression(cas.pointer());
            collect_from_expression(cas.old_value());
            collect_from_expression(cas.new_value());

            types.insert(cas.type_().clone());
        }
        Instruction::DeconstructRecord(deconstruct) => {
            collect_from_expression(deconstruct.record());

            types.insert(deconstruct.type_().clone().into());
        }
        Instruction::DeconstructUnion(deconstruct) => {
            collect_from_expression(deconstruct.union());

            types.insert(deconstruct.type_().clone().into());
        }
        Instruction::Fence(_) => {}
        Instruction::FreeHeap(free) => collect_from_expression(free.pointer()),
        Instruction::If(if_) => {
            collect_from_expression(if_.condition());
            collect_from_block(if_.then(), types);
            collect_from_block(if_.else_(), types);

            types.insert(if_.type_().clone());
        }
        Instruction::Load(load) => {
            collect_from_expression(load.pointer());

            types.insert(load.type_().clone());
        }
        Instruction::ReallocateHeap(reallocate) => {
            collect_from_expression(reallocate.pointer());
            collect_from_expression(reallocate.size());
        }
        Instruction::Store(store) => {
            collect_from_expression(store.value());
            collect_from_expression(store.pointer());

            types.insert(store.type_().clone());
        }
    }
}

fn collect_from_terminal_instruction(
    instruction: &TerminalInstruction,
    types: &mut FnvHashSet<Type>,
) {
    match instruction {
        TerminalInstruction::Branch(branch) => {
            types.insert(branch.type_().clone());
        }
        TerminalInstruction::Return(return_) => {
            types.insert(return_.type_().clone());
        }
        TerminalInstruction::Unreachable => {}
    }
}

fn collect_from_type(type_: &Type, types: &mut FnvHashSet<Type>) {
    types.insert(type_.clone());
    collect_child_types(type_, types);
}

fn collect_child_types(type_: &Type, types: &mut FnvHashSet<Type>) {
    let mut collect_from_type = |type_| collect_from_type(type_, types);

    match type_ {
        Type::Function(function) => {
            for argument in function.arguments() {
                collect_from_type(argument);
            }

            collect_from_type(function.result());
        }
        Type::Primitive(_) => {}
        Type::Record(record) => {
            for field in record.fields() {
                collect_from_type(field);
            }
        }
        Type::Pointer(pointer) => collect_from_type(pointer.element()),
        Type::Union(union) => {
            for member in union.members() {
                collect_from_type(member);
            }
        }
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
