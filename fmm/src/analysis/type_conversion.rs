use crate::ir::*;
use crate::types::{self, Type};

pub fn convert_types(module: &Module, convert: &impl Fn(&Type) -> Type) -> Module {
    let convert = |type_: &Type| -> Type { convert_type(type_, convert) };

    Module::new(
        module
            .variable_declarations()
            .iter()
            .map(|declaration| convert_variable_declaration(declaration, &convert))
            .collect(),
        module
            .function_declarations()
            .iter()
            .map(|declaration| convert_function_declaration(declaration, &convert))
            .collect(),
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

fn convert_variable_declaration(
    declaration: &VariableDeclaration,
    convert: &impl Fn(&Type) -> Type,
) -> VariableDeclaration {
    VariableDeclaration::new(declaration.name(), convert(declaration.type_()))
}

fn convert_function_declaration(
    declaration: &FunctionDeclaration,
    convert: &impl Fn(&Type) -> Type,
) -> FunctionDeclaration {
    FunctionDeclaration::new(
        declaration.name(),
        convert(&declaration.type_().clone().into())
            .to_function()
            .unwrap()
            .clone(),
    )
}

fn convert_variable_definition(
    definition: &VariableDefinition,
    convert: &impl Fn(&Type) -> Type,
) -> VariableDefinition {
    VariableDefinition::new(
        definition.name(),
        convert_expression(definition.body(), convert),
        convert(definition.type_()),
        definition.is_mutable(),
        definition.is_global(),
    )
}

fn convert_function_definition(
    definition: &FunctionDefinition,
    convert: &impl Fn(&Type) -> Type,
) -> FunctionDefinition {
    FunctionDefinition::new(
        definition.name(),
        definition
            .arguments()
            .iter()
            .map(|argument| Argument::new(argument.name(), convert(argument.type_())))
            .collect(),
        convert_block(definition.body(), convert),
        convert(definition.result_type()),
        definition.calling_convention(),
        definition.is_global(),
    )
}

fn convert_block(block: &Block, convert: &impl Fn(&Type) -> Type) -> Block {
    Block::new(
        block
            .instructions()
            .iter()
            .map(|instruction| convert_instruction(instruction, convert))
            .collect(),
        convert_terminal_instruction(block.terminal_instruction(), convert),
    )
}

fn convert_instruction(instruction: &Instruction, convert: &impl Fn(&Type) -> Type) -> Instruction {
    let convert_expression = |expression| convert_expression(expression, convert);

    match instruction {
        Instruction::AllocateHeap(allocate) => {
            AllocateHeap::new(convert(allocate.type_()), allocate.name()).into()
        }
        Instruction::AllocateStack(allocate) => {
            AllocateStack::new(convert(allocate.type_()), allocate.name()).into()
        }
        Instruction::ArithmeticOperation(operation) => ArithmeticOperation::new(
            convert(&operation.type_().into()).to_primitive().unwrap(),
            operation.operator(),
            convert_expression(operation.lhs()),
            convert_expression(operation.rhs()),
            operation.name(),
        )
        .into(),
        Instruction::AtomicLoad(load) => AtomicLoad::new(
            convert(load.type_()),
            convert_expression(load.pointer()),
            load.name(),
        )
        .into(),
        Instruction::AtomicOperation(operation) => AtomicOperation::new(
            convert(&operation.type_().into()).to_primitive().unwrap(),
            operation.operator(),
            convert_expression(operation.pointer()),
            convert_expression(operation.value()),
            operation.name(),
        )
        .into(),
        Instruction::AtomicStore(store) => AtomicStore::new(
            convert(store.type_()),
            convert_expression(store.value()),
            convert_expression(store.pointer()),
        )
        .into(),
        Instruction::Call(call) => Call::new(
            convert(&call.type_().clone().into())
                .to_function()
                .unwrap()
                .clone(),
            convert_expression(call.function()),
            call.arguments().iter().map(convert_expression).collect(),
            call.name(),
        )
        .into(),
        Instruction::CompareAndSwap(cas) => CompareAndSwap::new(
            convert(cas.type_()),
            convert_expression(cas.pointer()),
            convert_expression(cas.old_value()),
            convert_expression(cas.new_value()),
            cas.name(),
        )
        .into(),
        Instruction::ComparisonOperation(operation) => ComparisonOperation::new(
            convert(&operation.type_().into()).to_primitive().unwrap(),
            operation.operator(),
            convert_expression(operation.lhs()),
            convert_expression(operation.rhs()),
            operation.name(),
        )
        .into(),
        Instruction::DeconstructRecord(deconstruct) => DeconstructRecord::new(
            convert(&deconstruct.type_().clone().into())
                .to_record()
                .unwrap()
                .clone(),
            convert_expression(deconstruct.record()),
            deconstruct.element_index(),
            deconstruct.name(),
        )
        .into(),
        Instruction::DeconstructUnion(deconstruct) => DeconstructUnion::new(
            convert(&deconstruct.type_().clone().into())
                .to_union()
                .unwrap()
                .clone(),
            convert_expression(deconstruct.union()),
            deconstruct.member_index(),
            deconstruct.name(),
        )
        .into(),
        Instruction::If(if_) => If::new(
            convert(if_.type_()),
            convert_expression(if_.condition()),
            convert_block(if_.then(), convert),
            convert_block(if_.else_(), convert),
            if_.name(),
        )
        .into(),
        Instruction::Load(load) => Load::new(
            convert(load.type_()),
            convert_expression(load.pointer()),
            load.name(),
        )
        .into(),
        Instruction::PassThrough(pass) => PassThrough::new(
            convert(pass.type_()),
            convert_expression(pass.expression()),
            pass.name(),
        )
        .into(),
        Instruction::PointerAddress(address) => PointerAddress::new(
            convert(&address.type_().clone().into())
                .to_pointer()
                .unwrap()
                .clone(),
            convert_expression(address.pointer()),
            convert_expression(address.offset()),
            address.name(),
        )
        .into(),
        Instruction::ReallocateHeap(reallocate) => ReallocateHeap::new(
            convert_expression(reallocate.pointer()),
            convert_expression(reallocate.size()),
            reallocate.name(),
        )
        .into(),
        Instruction::RecordAddress(address) => RecordAddress::new(
            convert(&address.type_().clone().into())
                .to_record()
                .unwrap()
                .clone(),
            convert_expression(address.pointer()),
            address.element_index(),
            address.name(),
        )
        .into(),
        Instruction::Store(store) => Store::new(
            convert(store.type_()),
            convert_expression(store.value()),
            convert_expression(store.pointer()),
        )
        .into(),
        Instruction::UnionAddress(address) => UnionAddress::new(
            convert(&address.type_().clone().into())
                .to_union()
                .unwrap()
                .clone(),
            convert_expression(address.pointer()),
            address.member_index(),
            address.name(),
        )
        .into(),
    }
}

fn convert_terminal_instruction(
    instruction: &TerminalInstruction,
    convert: &impl Fn(&Type) -> Type,
) -> TerminalInstruction {
    let convert_expression = |expression| convert_expression(expression, convert);

    match instruction {
        TerminalInstruction::Branch(branch) => Branch::new(
            convert(branch.type_()),
            convert_expression(branch.expression()),
        )
        .into(),
        TerminalInstruction::Return(return_) => Return::new(
            convert(return_.type_()),
            convert_expression(return_.expression()),
        )
        .into(),
        TerminalInstruction::Unreachable => TerminalInstruction::Unreachable,
    }
}

fn convert_expression(expression: &Expression, convert: &impl Fn(&Type) -> Type) -> Expression {
    let convert_expression = |expression| convert_expression(expression, convert);

    match expression {
        Expression::AlignOf(align_of) => AlignOf::new(convert(align_of.type_())).into(),
        Expression::BitCast(bit_cast) => BitCast::new(
            convert(bit_cast.from()),
            convert(bit_cast.to()),
            convert_expression(bit_cast.expression()),
        )
        .into(),
        Expression::Record(record) => Record::new(
            convert(&record.type_().clone().into())
                .to_record()
                .unwrap()
                .clone(),
            record.elements().iter().map(convert_expression).collect(),
        )
        .into(),
        Expression::SizeOf(size_of) => SizeOf::new(convert(size_of.type_())).into(),
        Expression::Union(union) => Union::new(
            convert(&union.type_().clone().into())
                .to_union()
                .unwrap()
                .clone(),
            union.member_index(),
            convert_expression(union.member()),
        )
        .into(),
        Expression::Undefined(undefined) => Undefined::new(convert(undefined.type_())).into(),
        Expression::Primitive(_) | Expression::Variable(_) => expression.clone(),
    }
}

fn convert_type(type_: &Type, convert: &impl Fn(&Type) -> Type) -> Type {
    convert(&{
        let convert = |type_| convert_type(type_, convert);

        match type_ {
            Type::Function(function) => types::Function::new(
                function
                    .arguments()
                    .iter()
                    .map(|argument| convert(argument))
                    .collect(),
                convert(function.result()),
                function.calling_convention(),
            )
            .into(),
            Type::Primitive(_) => type_.clone(),
            Type::Record(record) => {
                types::Record::new(record.elements().iter().map(convert).collect()).into()
            }
            Type::Pointer(pointer) => types::Pointer::new(convert(pointer.element())).into(),
            Type::Union(union) => {
                types::Union::new(union.members().iter().map(convert).collect()).into()
            }
        }
    })
}
