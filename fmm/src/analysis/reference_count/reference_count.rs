use crate::build::{self, InstructionBuilder, NameGenerator};
use crate::ir::*;
use crate::types::{self, CallingConvention, Type};
use once_cell::sync::Lazy;
use std::rc::Rc;

static VOID_TYPE: Lazy<types::Record> = Lazy::new(|| types::Record::new(vec![]));
static VOID_VALUE: Lazy<Record> = Lazy::new(|| Record::new(VOID_TYPE.clone(), vec![]));

pub fn increment_count(variable: &Variable, type_: &Type) -> Vec<Instruction> {
    if let Type::Pointer(pointer_type) = type_ {
        let builder = InstructionBuilder::new(Rc::new(NameGenerator::new(variable.name()).into()));

        increment_pointer(&builder, variable, pointer_type);

        builder.into_instructions()
    } else {
        vec![]
    }
}

pub fn create_increment_function_definition() -> FunctionDefinition {
    let builder = InstructionBuilder::new(Rc::new(NameGenerator::new(variable.name()).into()));

    FunctionDefinition::new(
        "_rc_increment_count",
        vec![Argument::new(
            "pointer",
            types::Pointer::new(types::Primitive::PointerInteger),
        )],
        Block::new(
            builder.into_instructions(),
            Return::new(VOID_TYPE.clone(), VOID_VALUE.clone()),
        ),
        VOID_TYPE.clone(),
        CallingConvention::Target,
        false,
    )
}

fn increment_pointer(builder: &InstructionBuilder, variable: &Variable, type_: &types::Pointer) {
    let pointer = builder.pointer_address(
        build::bit_cast(
            types::Pointer::new(types::Primitive::PointerInteger),
            build::variable(variable.name(), type_.clone()),
        ),
        Primitive::PointerInteger(-1),
    );

    let old_count = builder.load(pointer.clone());
    let new_count = builder.arithmetic_operation(
        ArithmeticOperator::Add,
        old_count,
        Primitive::PointerInteger(1),
    );

    builder.compare_and_swap(pointer, old_count, new_count);
}

pub fn decrement_count(variable: &Variable, type_: &Type) -> Vec<Instruction> {
    todo!()
}
