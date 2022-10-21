mod error;

pub use self::error::TypeConversionError;
use crate::{
    ir::*,
    types::{self, Type},
};
use fnv::FnvHashMap;

pub fn convert(
    module: &mut Module,
    convert: &impl Fn(&Type) -> Type,
) -> Result<(), TypeConversionError> {
    let mut cache = FnvHashMap::default();
    let mut convert = |type_: &Type| -> Type { convert_type(type_, convert, &mut cache) };

    for variable_declaration in module.variable_declarations_mut() {
        convert_variable_declaration(variable_declaration, &mut convert);
    }

    for function_declaration in module.function_declarations_mut() {
        convert_function_declaration(function_declaration, &mut convert)?;
    }

    for variable_definition in module.variable_definitions_mut() {
        convert_variable_definition(variable_definition, &mut convert);
    }

    for function_definition in module.function_definitions_mut() {
        convert_function_definition(function_definition, &mut convert)?;
    }

    Ok(())
}

fn convert_variable_declaration(
    declaration: &mut VariableDeclaration,
    convert: &mut impl FnMut(&Type) -> Type,
) {
    *declaration.type_mut() = convert(declaration.type_());
}

fn convert_function_declaration(
    declaration: &mut FunctionDeclaration,
    convert: &mut impl FnMut(&Type) -> Type,
) -> Result<(), TypeConversionError> {
    *declaration.type_mut() = {
        let type_ = convert(&declaration.type_().clone().into());

        if let Type::Function(function) = type_ {
            function
        } else {
            return Err(TypeConversionError::FunctionExpected(type_));
        }
    };

    Ok(())
}

fn convert_variable_definition(
    definition: &mut VariableDefinition,
    convert: &mut impl FnMut(&Type) -> Type,
) {
    *definition.body_mut() = convert_expression(definition.body(), convert);
    *definition.type_mut() = convert(definition.type_());
}

fn convert_function_definition(
    definition: &mut FunctionDefinition,
    convert: &mut impl FnMut(&Type) -> Type,
) -> Result<(), TypeConversionError> {
    for argument in definition.arguments_mut() {
        *argument.type_mut() = convert(argument.type_());
    }

    *definition.result_type_mut() = convert(definition.result_type());

    convert_block(definition.body_mut(), convert)?;

    Ok(())
}

fn convert_block(
    block: &mut Block,
    convert: &mut impl FnMut(&Type) -> Type,
) -> Result<(), TypeConversionError> {
    for instruction in block.instructions_mut() {
        convert_instruction(instruction, convert)?;
    }

    convert_terminal_instruction(block.terminal_instruction_mut(), convert);

    Ok(())
}

fn convert_instruction(
    instruction: &mut Instruction,
    convert: &mut impl FnMut(&Type) -> Type,
) -> Result<(), TypeConversionError> {
    match instruction {
        Instruction::AllocateHeap(allocate) => {
            *allocate.size_mut() = convert_expression(allocate.size(), convert);
        }
        Instruction::AllocateStack(allocate) => {
            *allocate.type_mut() = convert(allocate.type_());
        }
        Instruction::AtomicLoad(load) => {
            *load.type_mut() = convert(load.type_());
            *load.pointer_mut() = convert_expression(load.pointer(), convert);
        }
        Instruction::AtomicOperation(operation) => {
            *operation.type_mut() = match convert(&operation.type_().into()) {
                Type::Primitive(primitive) => primitive,
                type_ => return Err(TypeConversionError::PrimitiveExpected(type_)),
            };
            *operation.pointer_mut() = convert_expression(operation.pointer(), convert);
            *operation.value_mut() = convert_expression(operation.value(), convert);
        }
        Instruction::AtomicStore(store) => {
            *store.type_mut() = convert(store.type_());
            *store.value_mut() = convert_expression(store.value(), convert);
            *store.pointer_mut() = convert_expression(store.pointer(), convert);
        }
        Instruction::Call(call) => {
            *call.type_mut() = match convert(&call.type_().clone().into()) {
                Type::Function(function) => function,
                type_ => return Err(TypeConversionError::FunctionExpected(type_)),
            };
            *call.function_mut() = convert_expression(call.function(), convert);

            for argument in call.arguments_mut() {
                convert_expression(argument, convert);
            }
        }
        Instruction::CompareAndSwap(cas) => {
            *cas.type_mut() = convert(cas.type_());
            *cas.pointer_mut() = convert_expression(cas.pointer(), convert);
            *cas.old_value_mut() = convert_expression(cas.old_value(), convert);
            *cas.new_value_mut() = convert_expression(cas.new_value(), convert);
        }
        Instruction::DeconstructRecord(deconstruct) => {
            *deconstruct.type_mut() = match convert(&deconstruct.type_().clone().into()) {
                Type::Record(record) => record,
                type_ => return Err(TypeConversionError::RecordExpected(type_)),
            };
            *deconstruct.record_mut() = convert_expression(deconstruct.record(), convert);
        }
        Instruction::DeconstructUnion(deconstruct) => {
            *deconstruct.type_mut() = match convert(&deconstruct.type_().clone().into()) {
                Type::Union(union) => union,
                type_ => return Err(TypeConversionError::UnionExpected(type_)),
            };
            *deconstruct.union_mut() = convert_expression(deconstruct.union(), convert);
        }
        Instruction::Fence(_) => {}
        Instruction::FreeHeap(free) => {
            *free.pointer_mut() = convert_expression(free.pointer(), convert);
        }
        Instruction::If(if_) => {
            *if_.type_mut() = convert(if_.type_());
            *if_.condition_mut() = convert_expression(if_.condition(), convert);
            convert_block(if_.then_mut(), convert)?;
            convert_block(if_.else_mut(), convert)?;
        }
        Instruction::Load(load) => {
            *load.type_mut() = convert(load.type_());
            *load.pointer_mut() = convert_expression(load.pointer(), convert);
        }
        Instruction::MemoryCopy(copy) => {
            *copy.source_mut() = convert_expression(copy.source(), convert);
            *copy.destination_mut() = convert_expression(copy.destination(), convert);
            *copy.size_mut() = convert_expression(copy.size(), convert);
        }
        Instruction::ReallocateHeap(reallocate) => {
            *reallocate.pointer_mut() = convert_expression(reallocate.pointer(), convert);
            *reallocate.size_mut() = convert_expression(reallocate.size(), convert);
        }
        Instruction::Store(store) => {
            *store.type_mut() = convert(store.type_());
            *store.value_mut() = convert_expression(store.value(), convert);
            *store.pointer_mut() = convert_expression(store.pointer(), convert);
        }
    }

    Ok(())
}

fn convert_terminal_instruction(
    instruction: &mut TerminalInstruction,
    convert: &mut impl FnMut(&Type) -> Type,
) {
    match instruction {
        TerminalInstruction::Branch(branch) => {
            *branch.type_mut() = convert(branch.type_());
            *branch.expression_mut() = convert_expression(branch.expression(), convert);
        }
        TerminalInstruction::Return(return_) => {
            *return_.type_mut() = convert(return_.type_());
            *return_.expression_mut() = convert_expression(return_.expression(), convert);
        }
        TerminalInstruction::Unreachable => {}
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

    fn convert_module(module: &Module, convert_type: &impl Fn(&Type) -> Type) -> Module {
        let mut module = module.clone();

        convert(&mut module, convert_type).unwrap();

        module
    }

    #[test]
    fn convert_empty() {
        let module = Module::new(vec![], vec![], vec![], vec![]);

        assert_eq!(
            convert_module(&module, &|_| types::Primitive::PointerInteger.into()),
            module
        );
    }

    #[test]
    fn convert_variable_declaration() {
        assert_eq!(
            convert_module(
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
            convert_module(
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
