use crate::{
    ir::*,
    types::{self, Type},
};
use fnv::FnvHashMap;

pub fn convert(module: &Module, convert: &impl Fn(&Type) -> Type) -> Module {
    let mut cache = FnvHashMap::default();
    let mut convert = |type_: &Type| -> Type { convert_type(type_, convert, &mut cache) };

    Module::new(
        module
            .variable_declarations()
            .iter()
            .map(|declaration| convert_variable_declaration(declaration, &mut convert))
            .collect(),
        module
            .function_declarations()
            .iter()
            .map(|declaration| convert_function_declaration(declaration, &mut convert))
            .collect(),
        module
            .variable_definitions()
            .iter()
            .map(|definition| convert_variable_definition(definition, &mut convert))
            .collect(),
        module
            .function_definitions()
            .iter()
            .map(|definition| convert_function_definition(definition, &mut convert))
            .collect(),
    )
}

fn convert_variable_declaration(
    declaration: &VariableDeclaration,
    convert: &mut impl FnMut(&Type) -> Type,
) -> VariableDeclaration {
    VariableDeclaration::new(declaration.name(), convert(declaration.type_()))
}

fn convert_function_declaration(
    declaration: &FunctionDeclaration,
    convert: &mut impl FnMut(&Type) -> Type,
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
    convert: &mut impl FnMut(&Type) -> Type,
) -> VariableDefinition {
    VariableDefinition::new(
        definition.name(),
        convert_expression(definition.body(), convert),
        convert(definition.type_()),
        definition.options().clone(),
    )
}

fn convert_function_definition(
    definition: &FunctionDefinition,
    convert: &mut impl FnMut(&Type) -> Type,
) -> FunctionDefinition {
    FunctionDefinition::new(
        definition.name(),
        definition
            .arguments()
            .iter()
            .map(|argument| Argument::new(argument.name(), convert(argument.type_())))
            .collect(),
        convert(definition.result_type()),
        convert_block(definition.body(), convert),
        definition.options().clone(),
    )
}

fn convert_block(block: &Block, convert: &mut impl FnMut(&Type) -> Type) -> Block {
    Block::new(
        block
            .instructions()
            .iter()
            .map(|instruction| convert_instruction(instruction, convert))
            .collect(),
        convert_terminal_instruction(block.terminal_instruction(), convert),
    )
}

fn convert_instruction(
    instruction: &Instruction,
    convert: &mut impl FnMut(&Type) -> Type,
) -> Instruction {
    match instruction {
        Instruction::AllocateHeap(allocate) => AllocateHeap::new(
            convert_expression(allocate.size(), convert),
            allocate.name(),
        )
        .into(),
        Instruction::AllocateStack(allocate) => {
            AllocateStack::new(convert(allocate.type_()), allocate.name()).into()
        }
        Instruction::AtomicLoad(load) => AtomicLoad::new(
            convert(load.type_()),
            convert_expression(load.pointer(), convert),
            load.ordering(),
            load.name(),
        )
        .into(),
        Instruction::AtomicOperation(operation) => AtomicOperation::new(
            convert(&operation.type_().into()).to_primitive().unwrap(),
            operation.operator(),
            convert_expression(operation.pointer(), convert),
            convert_expression(operation.value(), convert),
            operation.ordering(),
            operation.name(),
        )
        .into(),
        Instruction::AtomicStore(store) => AtomicStore::new(
            convert(store.type_()),
            convert_expression(store.value(), convert),
            convert_expression(store.pointer(), convert),
            store.ordering(),
        )
        .into(),
        Instruction::Call(call) => Call::new(
            convert(&call.type_().clone().into())
                .to_function()
                .unwrap()
                .clone(),
            convert_expression(call.function(), convert),
            call.arguments()
                .iter()
                .map(|expression| convert_expression(expression, convert))
                .collect(),
            call.name(),
        )
        .into(),
        Instruction::CompareAndSwap(cas) => CompareAndSwap::new(
            convert(cas.type_()),
            convert_expression(cas.pointer(), convert),
            convert_expression(cas.old_value(), convert),
            convert_expression(cas.new_value(), convert),
            cas.success_ordering(),
            cas.failure_ordering(),
            cas.name(),
        )
        .into(),
        Instruction::DeconstructRecord(deconstruct) => DeconstructRecord::new(
            convert(&deconstruct.type_().clone().into())
                .to_record()
                .unwrap()
                .clone(),
            convert_expression(deconstruct.record(), convert),
            deconstruct.field_index(),
            deconstruct.name(),
        )
        .into(),
        Instruction::DeconstructUnion(deconstruct) => DeconstructUnion::new(
            convert(&deconstruct.type_().clone().into())
                .to_union()
                .unwrap()
                .clone(),
            convert_expression(deconstruct.union(), convert),
            deconstruct.member_index(),
            deconstruct.name(),
        )
        .into(),
        Instruction::Fence(fence) => fence.clone().into(),
        Instruction::FreeHeap(free) => {
            FreeHeap::new(convert_expression(free.pointer(), convert)).into()
        }
        Instruction::If(if_) => If::new(
            convert(if_.type_()),
            convert_expression(if_.condition(), convert),
            convert_block(if_.then(), convert),
            convert_block(if_.else_(), convert),
            if_.name(),
        )
        .into(),
        Instruction::Load(load) => Load::new(
            convert(load.type_()),
            convert_expression(load.pointer(), convert),
            load.name(),
        )
        .into(),
        Instruction::MemoryCopy(copy) => MemoryCopy::new(
            convert_expression(copy.source(), convert),
            convert_expression(copy.destination(), convert),
            convert_expression(copy.size(), convert),
        )
        .into(),
        Instruction::ReallocateHeap(reallocate) => ReallocateHeap::new(
            convert_expression(reallocate.pointer(), convert),
            convert_expression(reallocate.size(), convert),
            reallocate.name(),
        )
        .into(),
        Instruction::Store(store) => Store::new(
            convert(store.type_()),
            convert_expression(store.value(), convert),
            convert_expression(store.pointer(), convert),
        )
        .into(),
    }
}

fn convert_terminal_instruction(
    instruction: &TerminalInstruction,
    convert: &mut impl FnMut(&Type) -> Type,
) -> TerminalInstruction {
    match instruction {
        TerminalInstruction::Branch(branch) => Branch::new(
            convert(branch.type_()),
            convert_expression(branch.expression(), convert),
        )
        .into(),
        TerminalInstruction::Return(return_) => Return::new(
            convert(return_.type_()),
            convert_expression(return_.expression(), convert),
        )
        .into(),
        TerminalInstruction::Unreachable => TerminalInstruction::Unreachable,
    }
}

fn convert_expression(
    expression: &Expression,
    convert: &mut impl FnMut(&Type) -> Type,
) -> Expression {
    match expression {
        Expression::AlignOf(align_of) => AlignOf::new(convert(align_of.type_())).into(),
        Expression::BitCast(bit_cast) => BitCast::new(
            convert(bit_cast.from()),
            convert(bit_cast.to()),
            convert_expression(bit_cast.expression(), convert),
        )
        .into(),
        Expression::ArithmeticOperation(operation) => ArithmeticOperation::new(
            convert(&operation.type_().into()).to_primitive().unwrap(),
            operation.operator(),
            convert_expression(operation.lhs(), convert),
            convert_expression(operation.rhs(), convert),
        )
        .into(),
        Expression::BitwiseNotOperation(operation) => BitwiseNotOperation::new(
            convert(&operation.type_().into()).to_primitive().unwrap(),
            convert_expression(operation.value(), convert),
        )
        .into(),
        Expression::BitwiseOperation(operation) => BitwiseOperation::new(
            convert(&operation.type_().into()).to_primitive().unwrap(),
            operation.operator(),
            convert_expression(operation.lhs(), convert),
            convert_expression(operation.rhs(), convert),
        )
        .into(),
        Expression::ComparisonOperation(operation) => ComparisonOperation::new(
            convert(&operation.type_().into()).to_primitive().unwrap(),
            operation.operator(),
            convert_expression(operation.lhs(), convert),
            convert_expression(operation.rhs(), convert),
        )
        .into(),
        Expression::PointerAddress(address) => PointerAddress::new(
            convert(&address.type_().clone().into())
                .to_pointer()
                .unwrap()
                .clone(),
            convert_expression(address.pointer(), convert),
            convert_expression(address.offset(), convert),
        )
        .into(),
        Expression::Record(record) => Record::new(
            convert(&record.type_().clone().into())
                .to_record()
                .unwrap()
                .clone(),
            record
                .fields()
                .iter()
                .map(|expression| convert_expression(expression, convert))
                .collect(),
        )
        .into(),
        Expression::RecordAddress(address) => RecordAddress::new(
            convert(&address.type_().clone().into())
                .to_record()
                .unwrap()
                .clone(),
            convert_expression(address.pointer(), convert),
            address.field_index(),
        )
        .into(),
        Expression::SizeOf(size_of) => SizeOf::new(convert(size_of.type_())).into(),
        Expression::Union(union) => Union::new(
            convert(&union.type_().clone().into())
                .to_union()
                .unwrap()
                .clone(),
            union.member_index(),
            convert_expression(union.member(), convert),
        )
        .into(),
        Expression::UnionAddress(address) => UnionAddress::new(
            convert(&address.type_().clone().into())
                .to_union()
                .unwrap()
                .clone(),
            convert_expression(address.pointer(), convert),
            address.member_index(),
        )
        .into(),
        Expression::Undefined(undefined) => Undefined::new(convert(undefined.type_())).into(),
        Expression::Primitive(_) | Expression::Variable(_) => expression.clone(),
    }
}

fn convert_type(
    type_: &Type,
    convert: &dyn Fn(&Type) -> Type,
    cache: &mut FnvHashMap<Type, Type>,
) -> Type {
    if let Some(type_) = cache.get(type_) {
        type_.clone()
    } else {
        let converted_type = convert(&{
            let mut convert = |type_| convert_type(type_, convert, cache);

            match type_ {
                Type::Function(function) => types::Function::new(
                    function.arguments().iter().map(&mut convert).collect(),
                    convert(function.result()),
                    function.calling_convention(),
                )
                .into(),
                Type::Primitive(_) => type_.clone(),
                Type::Record(record) => {
                    types::Record::new(record.fields().iter().map(convert).collect()).into()
                }
                Type::Pointer(pointer) => types::Pointer::new(convert(pointer.element())).into(),
                Type::Union(union) => {
                    types::Union::new(union.members().iter().map(convert).collect()).into()
                }
            }
        });

        cache.insert(type_.clone(), converted_type.clone());

        converted_type
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn convert_empty() {
        let module = Module::new(vec![], vec![], vec![], vec![]);

        assert_eq!(
            convert(&module, &|_| types::Primitive::PointerInteger.into()),
            module
        );
    }

    #[test]
    fn convert_variable_declaration() {
        assert_eq!(
            convert(
                &Module::new(
                    vec![VariableDeclaration::new("x", types::Primitive::Integer32)],
                    vec![],
                    vec![],
                    vec![],
                ),
                &|type_| match type_ {
                    Type::Primitive(types::Primitive::Integer32) =>
                        types::Primitive::PointerInteger.into(),
                    _ => type_.clone(),
                }
            ),
            Module::new(
                vec![VariableDeclaration::new(
                    "x",
                    types::Primitive::PointerInteger
                )],
                vec![],
                vec![],
                vec![],
            )
        );
    }

    #[test]
    fn convert_function_declaration() {
        assert_eq!(
            convert(
                &Module::new(
                    vec![],
                    vec![FunctionDeclaration::new(
                        "f",
                        types::Function::new(
                            vec![],
                            types::Primitive::Integer32,
                            types::CallingConvention::Target,
                        ),
                    )],
                    vec![],
                    vec![],
                ),
                &|type_| match type_ {
                    Type::Primitive(types::Primitive::Integer32) =>
                        types::Primitive::PointerInteger.into(),
                    _ => type_.clone(),
                }
            ),
            Module::new(
                vec![],
                vec![FunctionDeclaration::new(
                    "f",
                    types::Function::new(
                        vec![],
                        types::Primitive::PointerInteger,
                        types::CallingConvention::Target,
                    ),
                )],
                vec![],
                vec![],
            )
        );
    }
}
