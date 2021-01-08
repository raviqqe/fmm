use crate::expressions::*;
use crate::names::*;
use crate::types::*;
use fmm::ir::*;
use fmm::types;

pub fn compile_block(block: &Block) -> String {
    block
        .instructions()
        .iter()
        .map(compile_instruction)
        .chain(vec![compile_terminal_instruction(
            block.terminal_instruction(),
            None,
        )])
        .map(|string| "  ".to_owned() + &string)
        .collect::<Vec<_>>()
        .join("\n")
}

fn compile_instruction(instruction: &Instruction) -> String {
    match instruction {
        Instruction::AllocateHeap(allocate) => {
            format!(
                "{}=malloc(sizeof({}));",
                compile_typed_name(
                    &types::Pointer::new(allocate.type_().clone()).into(),
                    allocate.name()
                ),
                compile_type_id(allocate.type_())
            )
        }
        Instruction::ArithmeticOperation(_) => todo!(),
        Instruction::Assignment(assignment) => format!(
            "{}={};",
            compile_typed_name(&assignment.type_(), assignment.name()),
            compile_expression(assignment.expression())
        ),
        Instruction::AtomicLoad(load) => format!(
            "{}=({})atomic_load(({}){});",
            compile_typed_name(&load.type_(), load.name()),
            compile_type_id(load.type_()),
            compile_atomic_pointer_type_id(load.type_()),
            compile_expression(load.pointer()),
        ),
        Instruction::AtomicStore(store) => format!(
            "atomic_store(({}){},{});",
            compile_atomic_pointer_type_id(store.type_()),
            compile_expression(store.pointer()),
            compile_expression(store.value()),
        ),
        Instruction::Bitcast(_) => todo!(),
        Instruction::Call(call) => format!(
            "{}={}({});",
            compile_typed_name(call.type_().result(), call.name()),
            compile_expression(call.function()),
            call.arguments()
                .iter()
                .map(|argument| compile_expression(argument))
                .collect::<Vec<_>>()
                .join(",")
        ),
        Instruction::CompareAndSwap(_) => todo!(),
        Instruction::ComparisonOperation(_) => todo!(),
        Instruction::DeconstructRecord(_) => todo!(),
        Instruction::DeconstructUnion(_) => todo!(),
        Instruction::If(_) => todo!(),
        Instruction::Load(load) => format!(
            "{}=*{};",
            compile_typed_name(load.type_(), load.name()),
            compile_expression(load.pointer()),
        ),
        Instruction::PointerAddress(address) => format!(
            "{}={}+{};",
            compile_typed_name(&address.type_().clone().into(), address.name()),
            compile_expression(address.pointer()),
            compile_expression(address.offset()),
        ),
        Instruction::RecordAddress(address) => format!(
            "{}=&{}.{};",
            compile_typed_name(
                &types::Pointer::new(address.type_().elements()[address.element_index()].clone())
                    .into(),
                address.name(),
            ),
            compile_expression(address.pointer()),
            generate_record_element_name(address.element_index()),
        ),
        Instruction::Store(store) => format!(
            "*{}={};",
            compile_expression(store.pointer()),
            compile_expression(store.value()),
        ),
        Instruction::UnionAddress(address) => {
            format!(
                "{}=&{}.{};",
                compile_typed_name(
                    &types::Pointer::new(address.type_().members()[address.member_index()].clone())
                        .into(),
                    address.name(),
                ),
                compile_expression(address.pointer()),
                generate_union_member_name(address.member_index()),
            )
        }
    }
}

fn compile_terminal_instruction(
    instruction: &TerminalInstruction,
    block_variable_name: Option<&str>,
) -> String {
    match instruction {
        TerminalInstruction::Branch(branch) => block_variable_name
            .map(|name| format!("{}={};", name, compile_expression(branch.expression())))
            .unwrap_or_default(),
        TerminalInstruction::Return(return_) => {
            format!("return {};", compile_expression(return_.expression()))
        }
        TerminalInstruction::Unreachable => "abort();".into(),
    }
}
