use crate::{expression, name, type_};
use fmm::{ir::*, types::generic_pointer_type};
use fnv::{FnvHashMap, FnvHashSet};

pub fn compile_block(
    block: &Block,
    branch_variable_name: Option<&str>,
    global_variables: &FnvHashSet<String>,
    type_ids: &FnvHashMap<fmm::types::Type, String>,
) -> String {
    block
        .instructions()
        .iter()
        .map(|instruction| compile_instruction(instruction, global_variables, type_ids))
        .chain([compile_terminal_instruction(
            block.terminal_instruction(),
            branch_variable_name,
            global_variables,
            type_ids,
        )])
        .map(|string| "".to_owned() + &string)
        .collect::<Vec<_>>()
        .join("\n")
}

fn compile_instruction(
    instruction: &Instruction,
    global_variables: &FnvHashSet<String>,
    type_ids: &FnvHashMap<fmm::types::Type, String>,
) -> String {
    let compile_expression =
        |expression| expression::compile(expression, global_variables, type_ids);
    let compile_typed_name = |type_, name| type_::compile_name(type_, name, type_ids);
    let compile_type_id = |type_| type_::compile_id(type_, type_ids);

    match instruction {
        Instruction::AllocateHeap(allocate) => {
            format!(
                "{}=malloc({});",
                compile_typed_name(&generic_pointer_type(), allocate.name()),
                compile_expression(allocate.size()),
            )
        }
        Instruction::AllocateStack(allocate) => {
            let entity_name = allocate.name().to_owned() + "_entity";

            format!(
                "{};{}=&{};",
                compile_typed_name(allocate.type_(), &entity_name),
                compile_typed_name(
                    &fmm::types::Pointer::new(allocate.type_().clone()).into(),
                    allocate.name(),
                ),
                entity_name,
            )
        }
        Instruction::AtomicLoad(load) => format!(
            "{}=({})atomic_load_explicit(({}){},{});",
            compile_typed_name(load.type_(), load.name()),
            compile_type_id(load.type_()),
            type_::compile_atomic_pointer_id(load.type_(), type_ids),
            compile_expression(load.pointer()),
            compile_atomic_ordering(load.ordering()),
        ),
        Instruction::AtomicOperation(operation) => format!(
            "{}=atomic_fetch_{}_explicit(({}){},{},{});",
            compile_typed_name(&operation.type_().into(), operation.name()),
            match operation.operator() {
                AtomicOperator::Add => "add",
                AtomicOperator::Subtract => "sub",
            },
            type_::compile_atomic_pointer_id(&operation.type_().into(), type_ids),
            compile_expression(operation.pointer()),
            compile_expression(operation.value()),
            compile_atomic_ordering(operation.ordering()),
        ),
        Instruction::AtomicStore(store) => format!(
            "atomic_store_explicit(({}){},{},{});",
            type_::compile_atomic_pointer_id(store.type_(), type_ids),
            compile_expression(store.pointer()),
            compile_expression(store.value()),
            compile_atomic_ordering(store.ordering()),
        ),
        Instruction::Call(call) => format!(
            "{}={}({});",
            compile_typed_name(call.type_().result(), call.name()),
            compile_expression(call.function()),
            call.arguments()
                .iter()
                .map(compile_expression)
                .collect::<Vec<_>>()
                .join(",")
        ),
        Instruction::CompareAndSwap(cas) => {
            let name = "_cas_".to_owned() + cas.name();

            format!(
                "{}={};bool {}=atomic_compare_exchange_strong_explicit(({}){},&{},{},{},{});",
                compile_typed_name(cas.type_(), &name),
                compile_expression(cas.old_value()),
                cas.name(),
                type_::compile_atomic_pointer_id(cas.type_(), type_ids),
                compile_expression(cas.pointer()),
                name,
                compile_expression(cas.new_value()),
                compile_atomic_ordering(cas.success_ordering()),
                compile_atomic_ordering(cas.failure_ordering()),
            )
        }
        Instruction::DeconstructRecord(deconstruct) => format!(
            "{}={}.{};",
            compile_typed_name(
                &deconstruct.type_().fields()[deconstruct.field_index()],
                deconstruct.name(),
            ),
            compile_expression(deconstruct.record()),
            name::generate_record_field_name(deconstruct.field_index()),
        ),
        Instruction::DeconstructUnion(deconstruct) => format!(
            "{}={}.{};",
            compile_typed_name(
                &deconstruct.type_().members()[deconstruct.member_index()],
                deconstruct.name(),
            ),
            compile_expression(deconstruct.union()),
            name::generate_union_member_name(deconstruct.member_index()),
        ),
        Instruction::Fence(fence) => {
            format!(
                "atomic_thread_fence({});",
                compile_atomic_ordering(fence.ordering())
            )
        }
        Instruction::FreeHeap(free) => {
            format!("free((void *)({}));", compile_expression(free.pointer()))
        }
        Instruction::If(if_) => {
            let compile_block =
                |block| compile_block(block, Some(if_.name()), global_variables, type_ids);

            format!(
                "{};if({}){{\n{}\n}}else{{\n{}\n}}",
                compile_typed_name(&if_.type_().clone(), if_.name()),
                compile_expression(if_.condition()),
                compile_block(if_.then()),
                compile_block(if_.else_())
            )
        }
        Instruction::Load(load) => format!(
            "{}=*{};",
            compile_typed_name(load.type_(), load.name()),
            compile_expression(load.pointer()),
        ),
        Instruction::MemoryCopy(copy) => format!(
            "memcpy({},{},{});",
            compile_expression(copy.destination()),
            compile_expression(copy.source()),
            compile_expression(copy.size()),
        ),
        Instruction::ReallocateHeap(reallocate) => {
            format!(
                "{}=realloc({},{});",
                compile_typed_name(&fmm::types::generic_pointer_type(), reallocate.name()),
                compile_expression(reallocate.pointer()),
                compile_expression(reallocate.size()),
            )
        }
        Instruction::Store(store) => format!(
            "*{}={};",
            compile_expression(store.pointer()),
            compile_expression(store.value()),
        ),
    }
}

fn compile_terminal_instruction(
    instruction: &TerminalInstruction,
    block_variable_name: Option<&str>,
    global_variables: &FnvHashSet<String>,
    type_ids: &FnvHashMap<fmm::types::Type, String>,
) -> String {
    let compile_expression =
        |expression| expression::compile(expression, global_variables, type_ids);

    match instruction {
        TerminalInstruction::Branch(branch) => block_variable_name
            .map(|name| format!("{}={};", name, compile_expression(branch.expression(),)))
            .unwrap_or_default(),
        TerminalInstruction::Return(return_) => {
            format!("return {};", compile_expression(return_.expression(),))
        }
        TerminalInstruction::Unreachable => "abort();".into(),
    }
}

fn compile_atomic_ordering(ordering: AtomicOrdering) -> String {
    format!(
        "memory_order_{}",
        match ordering {
            AtomicOrdering::Relaxed => "relaxed",
            AtomicOrdering::Release => "release",
            AtomicOrdering::Acquire => "acquire",
            AtomicOrdering::AcquireRelease => "acq_rel",
            AtomicOrdering::SequentiallyConsistent => "seq_cst",
        }
    )
}
