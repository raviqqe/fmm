use crate::types::*;
use crate::{calling_convention::compile_calling_convention, heap::HeapFunctionSet};
use crate::{expressions::*, union::compile_union_cast};
use fmm::ir::*;
use inkwell::types::BasicType;
use inkwell::values::BasicValue;
use std::collections::HashMap;

pub fn compile_block<'c>(
    builder: &inkwell::builder::Builder<'c>,
    block: &Block,
    destination: Option<inkwell::basic_block::BasicBlock<'c>>,
    variables: &HashMap<String, inkwell::values::BasicValueEnum<'c>>,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
    heap_function_set: &HeapFunctionSet<'c>,
) -> Option<inkwell::values::BasicValueEnum<'c>> {
    let mut variables = variables.clone();

    for instruction in block.instructions() {
        let value = compile_instruction(
            builder,
            instruction,
            &variables,
            context,
            target_data,
            heap_function_set,
        );

        if let Some(value) = value {
            if let Some(name) = instruction.name() {
                variables.insert(name.into(), value);
            }
        }
    }

    compile_terminal_instruction(
        builder,
        block.terminal_instruction(),
        destination,
        &variables,
        context,
        target_data,
    )
}

fn compile_instruction<'c>(
    builder: &inkwell::builder::Builder<'c>,
    instruction: &Instruction,
    variables: &HashMap<String, inkwell::values::BasicValueEnum<'c>>,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
    heap_function_set: &HeapFunctionSet<'c>,
) -> Option<inkwell::values::BasicValueEnum<'c>> {
    let compile_expression =
        |expression| compile_expression(builder, expression, variables, context, target_data);
    let compile_type = |type_| compile_type(type_, context, target_data);

    match instruction {
        Instruction::AllocateHeap(allocate) => {
            let type_ = compile_type(allocate.type_());

            Some(
                builder.build_bitcast(
                    builder
                        .build_call(
                            heap_function_set.allocate_function,
                            &[compile_pointer_integer(
                                target_data.get_store_size(&type_) as u64,
                                context,
                                target_data,
                            )
                            .into()],
                            "",
                        )
                        .try_as_basic_value()
                        .left()
                        .unwrap(),
                    type_.ptr_type(DEFAULT_ADDRESS_SPACE),
                    allocate.name(),
                ),
            )
        }
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

            // TODO Optimize this.
            value
                .as_instruction_value()
                .unwrap()
                .set_atomic_ordering(inkwell::AtomicOrdering::SequentiallyConsistent)
                .unwrap();

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
                    inkwell::AtomicOrdering::SequentiallyConsistent,
                )
                .unwrap()
                .into(),
        ),
        Instruction::AtomicStore(store) => {
            let value = builder.build_store(
                compile_expression(store.pointer()).into_pointer_value(),
                compile_expression(store.value()),
            );

            // TODO Optimize this.
            value
                .set_atomic_ordering(inkwell::AtomicOrdering::SequentiallyConsistent)
                .unwrap();

            None
        }
        Instruction::Call(call) => {
            let value = builder.build_call(
                compile_expression(call.function()).into_pointer_value(),
                &call
                    .arguments()
                    .iter()
                    .map(|argument| compile_expression(argument))
                    .collect::<Vec<_>>(),
                call.name(),
            );

            value.set_tail_call(true);
            value.set_call_convention(compile_calling_convention(
                call.type_().calling_convention(),
            ));

            Some(value.try_as_basic_value().left().unwrap())
        }
        // TODO Optimize this.
        Instruction::CompareAndSwap(cas) => Some(
            builder
                .build_extract_value(
                    builder
                        .build_cmpxchg(
                            compile_expression(cas.pointer()).into_pointer_value(),
                            compile_expression(cas.old_value()),
                            compile_expression(cas.new_value()),
                            inkwell::AtomicOrdering::SequentiallyConsistent,
                            inkwell::AtomicOrdering::SequentiallyConsistent,
                        )
                        .unwrap(),
                    1,
                    cas.name(),
                )
                .unwrap(),
        ),
        Instruction::DeconstructRecord(deconstruct) => builder.build_extract_value(
            compile_expression(deconstruct.record()).into_struct_value(),
            deconstruct.element_index() as u32,
            deconstruct.name(),
        ),
        Instruction::DeconstructUnion(deconstruct) => builder.build_extract_value(
            compile_union_cast(
                builder,
                compile_expression(deconstruct.union()),
                compile_union_member_type(
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
        Instruction::FreeHeap(free) => {
            builder.build_call(
                heap_function_set.free_function,
                &[builder.build_bitcast(
                    compile_expression(free.pointer()),
                    context.i8_type().ptr_type(DEFAULT_ADDRESS_SPACE),
                    "",
                )],
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
                    heap_function_set,
                );

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
        Instruction::PassThrough(pass) => Some(builder.build_select(
            context.bool_type().const_int(1, false),
            compile_expression(pass.expression()),
            compile_expression(pass.expression()),
            pass.name(),
        )),
        Instruction::PointerAddress(address) => Some(unsafe {
            builder
                .build_gep(
                    compile_expression(address.pointer()).into_pointer_value(),
                    &[compile_expression(address.offset()).into_int_value()],
                    address.name(),
                )
                .into()
        }),
        Instruction::ReallocateHeap(reallocate) => builder
            .build_call(
                heap_function_set.reallocate_function,
                &[
                    compile_expression(reallocate.pointer()),
                    compile_expression(reallocate.size()),
                ],
                reallocate.name(),
            )
            .try_as_basic_value()
            .left(),
        Instruction::RecordAddress(address) => Some(unsafe {
            builder
                .build_gep(
                    compile_expression(address.pointer()).into_pointer_value(),
                    &[
                        context.i32_type().const_zero(),
                        context
                            .i32_type()
                            .const_int(address.element_index() as u64, false),
                    ],
                    address.name(),
                )
                .into()
        }),
        Instruction::Store(store) => {
            builder.build_store(
                compile_expression(store.pointer()).into_pointer_value(),
                compile_expression(store.value()),
            );

            None
        }
        Instruction::UnionAddress(address) => Some(unsafe {
            builder
                .build_gep(
                    builder
                        .build_bitcast(
                            compile_expression(address.pointer()),
                            compile_union_member_type(
                                address.type_(),
                                address.member_index(),
                                context,
                                target_data,
                            )
                            .ptr_type(DEFAULT_ADDRESS_SPACE),
                            "",
                        )
                        .into_pointer_value(),
                    &[
                        context.i32_type().const_zero(),
                        context.i32_type().const_zero(),
                    ],
                    address.name(),
                )
                .into()
        }),
    }
}

fn compile_terminal_instruction<'c>(
    builder: &inkwell::builder::Builder<'c>,
    instruction: &TerminalInstruction,
    destination: Option<inkwell::basic_block::BasicBlock<'c>>,
    variables: &HashMap<String, inkwell::values::BasicValueEnum<'c>>,
    context: &'c inkwell::context::Context,
    target_data: &inkwell::targets::TargetData,
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
            builder.build_unreachable();
            None
        }
    }
}
