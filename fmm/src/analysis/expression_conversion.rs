use crate::ir::*;

pub fn convert_expressions(
    module: &Module,
    convert: &impl Fn(&Expression) -> Expression,
) -> Module {
    let convert =
        |expression: &Expression| -> Expression { convert_expression(expression, convert) };

    Module::new(
        module.variable_declarations().to_vec(),
        module.function_declarations().to_vec(),
        module
            .variable_definitions()
            .iter()
            .map(|definition| convert_variable_definition(definition, &convert))
            .collect(),
        module
            .function_definitions()
            .iter()
            .map(|definition| convert_function_definition(definition, &convert))
            .collect(),
    )
}

fn convert_variable_definition(
    definition: &VariableDefinition,
    convert: &impl Fn(&Expression) -> Expression,
) -> VariableDefinition {
    VariableDefinition::new(
        definition.name(),
        convert(definition.body()),
        definition.type_().clone(),
        definition.is_mutable(),
        definition.is_global(),
    )
}

fn convert_function_definition(
    definition: &FunctionDefinition,
    convert: &impl Fn(&Expression) -> Expression,
) -> FunctionDefinition {
    FunctionDefinition::new(
        definition.name(),
        definition.arguments().to_vec(),
        convert_block(definition.body(), convert),
        definition.result_type().clone(),
        definition.calling_convention(),
        definition.is_global(),
    )
}

fn convert_block(block: &Block, convert: &impl Fn(&Expression) -> Expression) -> Block {
    Block::new(
        block
            .instructions()
            .iter()
            .map(|instruction| convert_expressions_in_instruction(instruction, convert))
            .collect(),
        convert_expressions_in_terminal_instruction(block.terminal_instruction(), convert),
    )
}

pub fn convert_expressions_in_instruction(
    instruction: &Instruction,
    convert: &impl Fn(&Expression) -> Expression,
) -> Instruction {
    match instruction {
        Instruction::AllocateHeap(allocate) => {
            AllocateHeap::new(allocate.type_().clone(), allocate.name()).into()
        }
        Instruction::AllocateStack(allocate) => {
            AllocateStack::new(allocate.type_().clone(), allocate.name()).into()
        }
        Instruction::ArithmeticOperation(operation) => ArithmeticOperation::new(
            operation.type_(),
            operation.operator(),
            convert(operation.lhs()),
            convert(operation.rhs()),
            operation.name(),
        )
        .into(),
        Instruction::AtomicLoad(load) => {
            AtomicLoad::new(load.type_().clone(), convert(load.pointer()), load.name()).into()
        }
        Instruction::AtomicStore(store) => AtomicStore::new(
            store.type_().clone(),
            convert(store.value()),
            convert(store.pointer()),
        )
        .into(),
        Instruction::Call(call) => Call::new(
            call.type_().clone(),
            convert(call.function()),
            call.arguments().iter().map(convert).collect(),
            call.name(),
        )
        .into(),
        Instruction::CompareAndSwap(cas) => CompareAndSwap::new(
            cas.type_().clone(),
            convert(cas.pointer()),
            convert(cas.old_value()),
            convert(cas.new_value()),
            cas.name(),
        )
        .into(),
        Instruction::ComparisonOperation(operation) => ComparisonOperation::new(
            operation.type_(),
            operation.operator(),
            convert(operation.lhs()),
            convert(operation.rhs()),
            operation.name(),
        )
        .into(),
        Instruction::DeconstructRecord(deconstruct) => DeconstructRecord::new(
            deconstruct.type_().clone(),
            convert(deconstruct.record()),
            deconstruct.element_index(),
            deconstruct.name(),
        )
        .into(),
        Instruction::DeconstructUnion(deconstruct) => DeconstructUnion::new(
            deconstruct.type_().clone(),
            convert(deconstruct.union()),
            deconstruct.member_index(),
            deconstruct.name(),
        )
        .into(),
        Instruction::If(if_) => If::new(
            if_.type_().clone(),
            convert(if_.condition()),
            convert_block(if_.then(), convert),
            convert_block(if_.else_(), convert),
            if_.name(),
        )
        .into(),
        Instruction::Load(load) => {
            Load::new(load.type_().clone(), convert(load.pointer()), load.name()).into()
        }
        Instruction::PassThrough(pass) => PassThrough::new(
            pass.type_().clone(),
            convert(pass.expression()),
            pass.name(),
        )
        .into(),
        Instruction::PointerAddress(address) => PointerAddress::new(
            address.type_().clone(),
            convert(address.pointer()),
            convert(address.offset()),
            address.name(),
        )
        .into(),
        Instruction::ReallocateHeap(reallocate) => ReallocateHeap::new(
            convert(reallocate.pointer()),
            convert(reallocate.size()),
            reallocate.name(),
        )
        .into(),
        Instruction::RecordAddress(address) => RecordAddress::new(
            address.type_().clone(),
            convert(address.pointer()),
            address.element_index(),
            address.name(),
        )
        .into(),
        Instruction::Store(store) => Store::new(
            store.type_().clone(),
            convert(store.value()),
            convert(store.pointer()),
        )
        .into(),
        Instruction::UnionAddress(address) => UnionAddress::new(
            address.type_().clone(),
            convert(address.pointer()),
            address.member_index(),
            address.name(),
        )
        .into(),
    }
}

pub fn convert_expressions_in_terminal_instruction(
    instruction: &TerminalInstruction,
    convert: &impl Fn(&Expression) -> Expression,
) -> TerminalInstruction {
    match instruction {
        TerminalInstruction::Branch(branch) => {
            Branch::new(branch.type_().clone(), convert(branch.expression())).into()
        }
        TerminalInstruction::Return(return_) => {
            Return::new(return_.type_().clone(), convert(return_.expression())).into()
        }
        TerminalInstruction::Unreachable => TerminalInstruction::Unreachable,
    }
}

fn convert_expression(
    expression: &Expression,
    convert: &impl Fn(&Expression) -> Expression,
) -> Expression {
    convert(&{
        let convert = |expression| convert_expression(expression, convert);

        match expression {
            Expression::BitCast(bit_cast) => BitCast::new(
                bit_cast.from().clone(),
                bit_cast.to().clone(),
                convert(bit_cast.expression()),
            )
            .into(),
            Expression::Record(record) => Record::new(
                record.type_().clone(),
                record.elements().iter().map(convert).collect(),
            )
            .into(),
            Expression::Union(union) => Union::new(
                union.type_().clone(),
                union.member_index(),
                convert(union.member()),
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
