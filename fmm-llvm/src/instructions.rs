use crate::{
    calling_convention::compile_calling_convention, error::CompileError, expressions::*,
    instruction_configuration::InstructionFunctionSet, types, union::compile_union_cast,
};
use fmm::ir::*;
use inkwell::values::BasicValue;
use std::convert::TryFrom;

pub fn compile_block<'c>(
    builder: &inkwell::builder::Builder<'c>,
    block: &Block,
    destination: Option<inkwell::basic_block::BasicBlock<'c>>,
    variables: &hamt::Map<String, inkwell::values::BasicValueEnum<'c>>,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
    instruction_function_set: &InstructionFunctionSet<'c>,
) -> Result<Option<inkwell::values::BasicValueEnum<'c>>, CompileError> {
    let mut variables = variables.clone();

    for instruction in block.instructions() {
        let value = compile_instruction(
            builder,
            instruction,
            &variables,
            context,
            target_data,
            instruction_function_set,
        )?;

        if let Some(value) = value {
            if let Some((name, _)) = instruction.value() {
                variables = variables.insert(name.into(), value);
            }
        }
    }

    Ok(compile_terminal_instruction(
        builder,
        block.terminal_instruction(),
        destination,
        &variables,
        context,
        target_data,
        instruction_function_set,
    ))
}

fn compile_instruction<'c>(
    builder: &inkwell::builder::Builder<'c>,
    instruction: &Instruction,
    variables: &hamt::Map<String, inkwell::values::BasicValueEnum<'c>>,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
    instruction_function_set: &InstructionFunctionSet<'c>,
) -> Result<Option<inkwell::values::BasicValueEnum<'c>>, CompileError> {
    let compile_expression =
        |expression| compile_expression(builder, expression, variables, context, target_data);
    let compile_type = |type_| types::compile(type_, context, target_data);

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

            value
                .as_instruction_value()
                .unwrap()
                .set_atomic_ordering(compile_atomic_ordering(load.ordering()))?;

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
            value.set_call_convention(compile_calling_convention(
                call.type_().calling_convention(),
            )?);

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
                types::compile_union_member(
                    deconstruct.type_(),
                    deconstruct.member_index(),
                    context,
                    target_data,
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
                        context.i8_type().ptr_type(types::DEFAULT_ADDRESS_SPACE),
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

            let then = context.append_basic_block(function, "then");
            let else_ = context.append_basic_block(function, "else");
            let phi = context.append_basic_block(function, "phi");

            let mut cases = vec![];

            builder.build_conditional_branch(
                compile_expression(if_.condition()).into_int_value(),
                then,
                else_,
            );

            for (llvm_block, block) in &[(then, if_.then()), (else_, if_.else_())] {
                builder.position_at_end(*llvm_block);

                let value = compile_block(
                    builder,
                    block,
                    Some(phi),
                    variables,
                    context,
                    target_data,
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
    builder: &inkwell::builder::Builder<'c>,
    instruction: &TerminalInstruction,
    destination: Option<inkwell::basic_block::BasicBlock<'c>>,
    variables: &hamt::Map<String, inkwell::values::BasicValueEnum<'c>>,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
    instruction_function_set: &InstructionFunctionSet<'c>,
) -> Option<inkwell::values::BasicValueEnum<'c>> {
    let compile_expression =
        |expression| compile_expression(builder, expression, variables, context, target_data);

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
