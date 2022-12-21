use crate::{
    calling_convention, context::Context, error::CompileError, expression,
    instruction_configuration::InstructionFunctionSet, type_, union::compile_union_cast,
};
use fmm::ir::*;
use fnv::FnvHashMap;
use inkwell::values::BasicValue;
use std::convert::TryFrom;

pub fn compile_block<'c, 'a>(
    context: &Context<'c>,
    builder: &inkwell::builder::Builder<'c>,
    block: &'a Block,
    destination: Option<inkwell::basic_block::BasicBlock<'c>>,
    variables: &mut FnvHashMap<&'a str, inkwell::values::BasicValueEnum<'c>>,
    instruction_function_set: &InstructionFunctionSet<'c>,
) -> Result<Option<inkwell::values::BasicValueEnum<'c>>, CompileError> {
    for instruction in block.instructions() {
        let value = compile_instruction(
            context,
            builder,
            instruction,
            variables,
            instruction_function_set,
        )?;

        if let Some(value) = value {
            if let Some((name, _)) = instruction.value() {
                variables.insert(name, value);
            }
        }
    }

    Ok(compile_terminal_instruction(
        context,
        builder,
        block.terminal_instruction(),
        destination,
        variables,
        instruction_function_set,
    ))
}

fn compile_instruction<'c, 'a>(
    context: &Context<'c>,
    builder: &inkwell::builder::Builder<'c>,
    instruction: &'a Instruction,
    variables: &mut FnvHashMap<&'a str, inkwell::values::BasicValueEnum<'c>>,
    instruction_function_set: &InstructionFunctionSet<'c>,
) -> Result<Option<inkwell::values::BasicValueEnum<'c>>, CompileError> {
    let compile_expression =
        |expression| expression::compile(context, builder, expression, variables);
    let compile_type = |type_| type_::compile(context, type_);

    Ok(match instruction {
        Instruction::AllocateHeap(allocate) => Some(
            builder
                .build_call(
                    instruction_function_set.allocate_function,
                    &[compile_expression(allocate.size()).into()],
                    allocate.name(),
                )
                .try_as_basic_value()
                .left()
                .unwrap(),
        ),
        Instruction::AllocateStack(allocate) => Some(
            builder
                .build_alloca(compile_type(allocate.type_()), allocate.name())
                .into(),
        ),
        Instruction::AtomicLoad(load) => {
            let value = builder.build_load(
                compile_expression(load.pointer()).into_pointer_value(),
                load.name(),
            );

            let instruction_value = value.as_instruction_value().unwrap();
            instruction_value.set_atomic_ordering(compile_atomic_ordering(load.ordering()))?;
            set_alignment(context, &instruction_value, load.type_())?;

            Some(value)
        }
        Instruction::AtomicOperation(operation) => Some(
            builder
                .build_atomicrmw(
                    match operation.operator() {
                        AtomicOperator::Add => inkwell::AtomicRMWBinOp::Add,
                        AtomicOperator::Subtract => inkwell::AtomicRMWBinOp::Sub,
                    },
                    compile_expression(operation.pointer()).into_pointer_value(),
                    compile_expression(operation.value()).into_int_value(),
                    compile_atomic_ordering(operation.ordering()),
                )?
                .into(),
        ),
        Instruction::AtomicStore(store) => {
            let value = builder.build_store(
                compile_expression(store.pointer()).into_pointer_value(),
                compile_expression(store.value()),
            );

            value.set_atomic_ordering(compile_atomic_ordering(store.ordering()))?;
            set_alignment(context, &value, store.type_())?;

            None
        }
        Instruction::Call(call) => {
            let value = builder.build_call(
                inkwell::values::CallableValue::try_from(
                    compile_expression(call.function()).into_pointer_value(),
                )
                .unwrap(),
                &call
                    .arguments()
                    .iter()
                    .map(|expression| compile_expression(expression).into())
                    .collect::<Vec<_>>(),
                call.name(),
            );

            value.set_tail_call(true);
            value.set_call_convention(calling_convention::compile(
                call.type_().calling_convention(),
            ));

            Some(value.try_as_basic_value().left().unwrap())
        }
        Instruction::CompareAndSwap(cas) => Some(
            builder
                .build_extract_value(
                    builder.build_cmpxchg(
                        compile_expression(cas.pointer()).into_pointer_value(),
                        compile_expression(cas.old_value()),
                        compile_expression(cas.new_value()),
                        compile_atomic_ordering(cas.success_ordering()),
                        compile_atomic_ordering(cas.failure_ordering()),
                    )?,
                    1,
                    cas.name(),
                )
                .unwrap(),
        ),
        Instruction::DeconstructRecord(deconstruct) => builder.build_extract_value(
            compile_expression(deconstruct.record()).into_struct_value(),
            deconstruct.field_index() as u32,
            deconstruct.name(),
        ),
        Instruction::DeconstructUnion(deconstruct) => builder.build_extract_value(
            compile_union_cast(
                builder,
                compile_expression(deconstruct.union()),
                type_::compile_union_member(
                    context,
                    deconstruct.type_(),
                    deconstruct.member_index(),
                )
                .into(),
            )
            .into_struct_value(),
            0,
            deconstruct.name(),
        ),
        Instruction::Fence(fence) => {
            builder.build_fence(compile_atomic_ordering(fence.ordering()), 0, "");

            None
        }
        Instruction::FreeHeap(free) => {
            builder.build_call(
                instruction_function_set.free_function,
                &[builder
                    .build_bitcast(
                        compile_expression(free.pointer()),
                        context
                            .inkwell()
                            .i8_type()
                            .ptr_type(Default::default()),
                        "",
                    )
                    .into()],
                "",
            );

            None
        }
        Instruction::If(if_) => {
            let current = builder.get_insert_block().unwrap();
            let function = current.get_parent().unwrap();

            let then = context.inkwell().append_basic_block(function, "then");
            let else_ = context.inkwell().append_basic_block(function, "else");
            let phi = context.inkwell().append_basic_block(function, "phi");

            let mut cases = vec![];

            builder.build_conditional_branch(
                compile_expression(if_.condition()).into_int_value(),
                then,
                else_,
            );

            for (llvm_block, block) in &[(then, if_.then()), (else_, if_.else_())] {
                builder.position_at_end(*llvm_block);

                let value = compile_block(
                    context,
                    builder,
                    block,
                    Some(phi),
                    variables,
                    instruction_function_set,
                )?;

                if let Some(value) = value {
                    cases.push((value, builder.get_insert_block().unwrap()));
                }
            }

            builder.position_at_end(phi);

            if cases.is_empty() {
                None
            } else {
                let phi = builder.build_phi(compile_type(if_.type_()), if_.name());

                phi.add_incoming(
                    &cases
                        .iter()
                        .map(|(value, block)| (value as &dyn inkwell::values::BasicValue, *block))
                        .collect::<Vec<_>>(),
                );

                Some(phi.as_basic_value())
            }
        }
        Instruction::Load(load) => Some(builder.build_load(
            compile_expression(load.pointer()).into_pointer_value(),
            load.name(),
        )),
        Instruction::MemoryCopy(copy) => {
            builder.build_memcpy(
                compile_expression(copy.destination()).into_pointer_value(),
                1,
                compile_expression(copy.source()).into_pointer_value(),
                1,
                compile_expression(copy.size()).into_int_value(),
            )?;

            None
        }
        Instruction::ReallocateHeap(reallocate) => builder
            .build_call(
                instruction_function_set.reallocate_function,
                &[
                    compile_expression(reallocate.pointer()).into(),
                    compile_expression(reallocate.size()).into(),
                ],
                reallocate.name(),
            )
            .try_as_basic_value()
            .left(),
        Instruction::Store(store) => {
            builder.build_store(
                compile_expression(store.pointer()).into_pointer_value(),
                compile_expression(store.value()),
            );

            None
        }
    })
}

fn compile_terminal_instruction<'c>(
    context: &Context<'c>,
    builder: &inkwell::builder::Builder<'c>,
    instruction: &TerminalInstruction,
    destination: Option<inkwell::basic_block::BasicBlock<'c>>,
    variables: &FnvHashMap<&str, inkwell::values::BasicValueEnum<'c>>,
    instruction_function_set: &InstructionFunctionSet<'c>,
) -> Option<inkwell::values::BasicValueEnum<'c>> {
    let compile_expression =
        |expression| expression::compile(context, builder, expression, variables);

    match instruction {
        TerminalInstruction::Branch(branch) => {
            let value = compile_expression(branch.expression());

            builder.build_unconditional_branch(destination.unwrap());

            Some(value)
        }
        TerminalInstruction::Return(return_) => {
            builder.build_return(Some(&compile_expression(return_.expression())));
            None
        }
        TerminalInstruction::Unreachable => {
            if let Some(function) = instruction_function_set.unreachable_function {
                builder.build_call(function, &[], "");
            }

            builder.build_unreachable();

            None
        }
    }
}

fn compile_atomic_ordering(ordering: AtomicOrdering) -> inkwell::AtomicOrdering {
    match ordering {
        AtomicOrdering::Relaxed => inkwell::AtomicOrdering::Monotonic,
        AtomicOrdering::Release => inkwell::AtomicOrdering::Release,
        AtomicOrdering::Acquire => inkwell::AtomicOrdering::Acquire,
        AtomicOrdering::AcquireRelease => inkwell::AtomicOrdering::AcquireRelease,
        AtomicOrdering::SequentiallyConsistent => inkwell::AtomicOrdering::SequentiallyConsistent,
    }
}

// Somehow sometimes, alignments of atomic operations are not properly set.
// For example, an alignment for i64 is 4 bytes instead of 8 bytes on
// x86_64-unknown-linux-gnu.
fn set_alignment(
    context: &Context,
    value: &inkwell::values::InstructionValue,
    type_: &fmm::types::Type,
) -> Result<(), &'static str> {
    value.set_alignment(
        context
            .target_data()
            .get_abi_alignment(&type_::compile(context, type_)),
    )
}
