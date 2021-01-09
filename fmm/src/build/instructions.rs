use super::instruction_context::*;
use super::names::*;
use crate::ir::*;
use crate::types::{self, Type};

pub fn arithmetic_operation(
    type_: types::Primitive,
    operator: ArithmeticOperator,
    lhs: impl Into<InstructionContext>,
    rhs: impl Into<InstructionContext>,
) -> InstructionContext {
    let lhs = lhs.into();
    let rhs = rhs.into();
    let name = generate_name();

    InstructionContext::new(
        lhs.instructions()
            .iter()
            .chain(rhs.instructions())
            .cloned()
            .chain(vec![ArithmeticOperation::new(
                type_,
                operator,
                lhs.expression().clone(),
                rhs.expression().clone(),
                &name,
            )
            .into()]),
        Variable::new(name),
    )
}

pub fn atomic_load(
    type_: impl Into<Type>,
    pointer: impl Into<InstructionContext>,
) -> InstructionContext {
    let pointer = pointer.into();
    let name = generate_name();

    InstructionContext::new(
        pointer
            .instructions()
            .iter()
            .cloned()
            .chain(vec![AtomicLoad::new(
                type_,
                pointer.expression().clone(),
                &name,
            )
            .into()]),
        Variable::new(name),
    )
}

pub fn call(
    type_: types::Function,
    function: impl Into<InstructionContext>,
    arguments: impl IntoIterator<Item = InstructionContext>,
) -> InstructionContext {
    let function = function.into();
    let arguments = arguments.into_iter().collect::<Vec<_>>();
    let name = generate_name();

    InstructionContext::new(
        function
            .instructions()
            .iter()
            .chain(arguments.iter().flat_map(|context| context.instructions()))
            .cloned()
            .chain(vec![Call::new(
                type_,
                function.expression().clone(),
                arguments
                    .iter()
                    .map(|context| context.expression())
                    .cloned()
                    .collect(),
                &name,
            )
            .into()]),
        Variable::new(name),
    )
}

pub fn comparison_operation(
    type_: types::Primitive,
    operator: ComparisonOperator,
    lhs: impl Into<InstructionContext>,
    rhs: impl Into<InstructionContext>,
) -> InstructionContext {
    let lhs = lhs.into();
    let rhs = rhs.into();
    let name = generate_name();

    InstructionContext::new(
        lhs.instructions()
            .iter()
            .chain(rhs.instructions())
            .cloned()
            .chain(vec![ComparisonOperation::new(
                type_,
                operator,
                lhs.expression().clone(),
                rhs.expression().clone(),
                &name,
            )
            .into()]),
        Variable::new(name),
    )
}

pub fn deconstruct_record(
    type_: types::Record,
    record: impl Into<InstructionContext>,
    element_index: usize,
) -> InstructionContext {
    let record = record.into();
    let name = generate_name();

    InstructionContext::new(
        record
            .instructions()
            .iter()
            .cloned()
            .chain(vec![DeconstructRecord::new(
                type_,
                record.expression().clone(),
                element_index,
                &name,
            )
            .into()]),
        Variable::new(name),
    )
}

pub fn deconstruct_union(
    type_: types::Union,
    union: impl Into<InstructionContext>,
    member_index: usize,
) -> InstructionContext {
    let union = union.into();
    let name = generate_name();

    InstructionContext::new(
        union
            .instructions()
            .iter()
            .cloned()
            .chain(vec![DeconstructUnion::new(
                type_,
                union.expression().clone(),
                member_index,
                &name,
            )
            .into()]),
        Variable::new(name),
    )
}

pub fn load(type_: impl Into<Type>, pointer: impl Into<InstructionContext>) -> InstructionContext {
    let pointer = pointer.into();
    let name = generate_name();

    InstructionContext::new(
        pointer.instructions().iter().cloned().chain(vec![Load::new(
            type_,
            pointer.expression().clone(),
            &name,
        )
        .into()]),
        Variable::new(name),
    )
}

pub fn pointer_address(
    type_: types::Pointer,
    pointer: impl Into<InstructionContext>,
    offset: impl Into<InstructionContext>,
) -> InstructionContext {
    let pointer = pointer.into();
    let offset = offset.into();
    let name = generate_name();

    InstructionContext::new(
        pointer
            .instructions()
            .iter()
            .chain(offset.instructions())
            .cloned()
            .chain(vec![PointerAddress::new(
                type_,
                pointer.expression().clone(),
                offset.expression().clone(),
                &name,
            )
            .into()]),
        Variable::new(name),
    )
}

pub fn record_address(
    type_: types::Record,
    pointer: impl Into<InstructionContext>,
    element_index: usize,
) -> InstructionContext {
    let pointer = pointer.into();
    let name = generate_name();

    InstructionContext::new(
        pointer
            .instructions()
            .iter()
            .cloned()
            .chain(vec![RecordAddress::new(
                type_,
                pointer.expression().clone(),
                element_index,
                &name,
            )
            .into()]),
        Variable::new(name),
    )
}

pub fn union_address(
    type_: types::Union,
    pointer: impl Into<InstructionContext>,
    member_index: usize,
) -> InstructionContext {
    let pointer = pointer.into();
    let name = generate_name();

    InstructionContext::new(
        pointer
            .instructions()
            .iter()
            .cloned()
            .chain(vec![UnionAddress::new(
                type_,
                pointer.expression().clone(),
                member_index,
                &name,
            )
            .into()]),
        Variable::new(name),
    )
}
