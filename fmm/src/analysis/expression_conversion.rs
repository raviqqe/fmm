use crate::ir::*;

pub fn convert(module: &mut Module, convert: &impl Fn(&Expression) -> Expression) {
    let convert = |expression: &mut _| *expression = convert_expression(expression, convert);

    for definition in module.variable_definitions_mut() {
        convert_variable_definition(definition, &convert);
    }

    for definition in module.function_definitions_mut() {
        convert_function_definition(definition, &convert);
    }
}

pub fn convert_in_instruction(
    instruction: &mut Instruction,
    convert: &impl Fn(&Expression) -> Expression,
) {
    convert_instruction(instruction, &|expression| {
        *expression = convert_expression(expression, convert)
    })
}

pub fn convert_in_terminal_instruction(
    instruction: &mut TerminalInstruction,
    convert: &impl Fn(&Expression) -> Expression,
) {
    convert_terminal_instruction(instruction, &|expression| {
        *expression = convert_expression(expression, convert)
    })
}

fn convert_variable_definition(
    definition: &mut VariableDefinition,
    convert: &impl Fn(&mut Expression),
) {
    convert(definition.body_mut());
}

fn convert_function_definition(
    definition: &mut FunctionDefinition,
    convert: &impl Fn(&mut Expression),
) {
    convert_block(definition.body_mut(), convert);
}

fn convert_block(block: &mut Block, convert: &impl Fn(&mut Expression)) {
    for instruction in block.instructions_mut() {
        convert_instruction(instruction, convert);
    }

    convert_terminal_instruction(block.terminal_instruction_mut(), convert);
}

fn convert_instruction(instruction: &mut Instruction, convert: &impl Fn(&mut Expression)) {
    match instruction {
        Instruction::AllocateHeap(allocate) => convert(allocate.size_mut()),
        Instruction::AllocateStack(_) => {}
        Instruction::AtomicLoad(load) => convert(load.pointer_mut()),
        Instruction::AtomicOperation(operation) => {
            convert(operation.pointer_mut());
            convert(operation.value_mut());
        }
        Instruction::AtomicStore(store) => {
            convert(store.value_mut());
            convert(store.pointer_mut());
        }
        Instruction::Call(call) => {
            convert(call.function_mut());

            for argument in call.arguments_mut() {
                convert(argument);
            }
        }
        Instruction::CompareAndSwap(cas) => {
            convert(cas.pointer_mut());
            convert(cas.old_value_mut());
            convert(cas.new_value_mut());
        }
        Instruction::DeconstructRecord(deconstruct) => convert(deconstruct.record_mut()),
        Instruction::DeconstructUnion(deconstruct) => convert(deconstruct.union_mut()),
        Instruction::Fence(_) => {}
        Instruction::FreeHeap(free) => convert(free.pointer_mut()),
        Instruction::If(if_) => {
            convert(if_.condition_mut());
            convert_block(if_.then_mut(), convert);
            convert_block(if_.else_mut(), convert);
        }
        Instruction::Load(load) => convert(load.pointer_mut()),
        Instruction::MemoryCopy(copy) => {
            convert(copy.source_mut());
            convert(copy.destination_mut());
            convert(copy.size_mut());
        }
        Instruction::ReallocateHeap(reallocate) => {
            convert(reallocate.pointer_mut());
            convert(reallocate.size_mut());
        }
        Instruction::Store(store) => {
            convert(store.value_mut());
            convert(store.pointer_mut());
        }
    }
}

fn convert_terminal_instruction(
    instruction: &mut TerminalInstruction,
    convert: &impl Fn(&mut Expression),
) {
    match instruction {
        TerminalInstruction::Branch(branch) => convert(branch.expression_mut()),
        TerminalInstruction::Return(return_) => convert(return_.expression_mut()),
        TerminalInstruction::Unreachable => {}
    }
}

fn convert_expression(
    expression: &Expression,
    convert: &impl Fn(&Expression) -> Expression,
) -> Expression {
    convert(&{
        let convert = |expression| convert_expression(expression, convert);

        match expression {
            Expression::ArithmeticOperation(operation) => ArithmeticOperation::new(
                operation.type_(),
                operation.operator(),
                convert(operation.lhs()),
                convert(operation.rhs()),
            )
            .into(),
            Expression::BitCast(bit_cast) => BitCast::new(
                bit_cast.from().clone(),
                bit_cast.to().clone(),
                convert(bit_cast.expression()),
            )
            .into(),
            Expression::BitwiseNotOperation(operation) => {
                BitwiseNotOperation::new(operation.type_(), convert(operation.value())).into()
            }
            Expression::BitwiseOperation(operation) => BitwiseOperation::new(
                operation.type_(),
                operation.operator(),
                convert(operation.lhs()),
                convert(operation.rhs()),
            )
            .into(),
            Expression::ComparisonOperation(operation) => ComparisonOperation::new(
                operation.type_(),
                operation.operator(),
                convert(operation.lhs()),
                convert(operation.rhs()),
            )
            .into(),
            Expression::PointerAddress(address) => PointerAddress::new(
                address.type_().clone(),
                convert(address.pointer()),
                convert(address.offset()),
            )
            .into(),
            Expression::Record(record) => Record::new(
                record.type_().clone(),
                record.fields().iter().map(convert).collect(),
            )
            .into(),
            Expression::RecordAddress(address) => RecordAddress::new(
                address.type_().clone(),
                convert(address.pointer()),
                address.field_index(),
            )
            .into(),
            Expression::Union(union) => Union::new(
                union.type_().clone(),
                union.member_index(),
                convert(union.member()),
            )
            .into(),
            Expression::UnionAddress(address) => UnionAddress::new(
                address.type_().clone(),
                convert(address.pointer()),
                address.member_index(),
            )
            .into(),
            Expression::AlignOf(_)
            | Expression::Primitive(_)
            | Expression::SizeOf(_)
            | Expression::Undefined(_)
            | Expression::Variable(_) => expression.clone(),
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types;

    #[test]
    fn convert_deconstruct_union() {
        let union_type = types::Union::new(vec![
            types::Primitive::Float64.into(),
            types::Primitive::Integer64.into(),
        ]);
        let mut instruction = DeconstructUnion::new(
            union_type.clone(),
            Union::new(union_type.clone(), 0, Variable::new("foo")),
            1,
            "x",
        )
        .into();

        convert_in_instruction(&mut instruction, &|expression| match expression {
            Expression::Variable(variable) => {
                if variable.name() == "foo" {
                    Variable::new("bar").into()
                } else {
                    expression.clone()
                }
            }
            _ => expression.clone(),
        });

        pretty_assertions::assert_eq!(
            instruction,
            DeconstructUnion::new(
                union_type.clone(),
                Union::new(union_type, 0, Variable::new("bar")),
                1,
                "x"
            )
            .into()
        );
    }
}
