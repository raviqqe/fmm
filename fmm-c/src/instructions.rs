use crate::expressions::*;
use crate::names::*;
use crate::types::*;
use fmm::ir::*;
use fmm::types;
use std::collections::{HashMap, HashSet};

pub fn compile_block(
    block: &Block,
    branch_variable_name: Option<&str>,
    global_variables: &HashSet<String>,
    type_ids: &HashMap<fmm::types::Type, String>,
) -> String {
    block
        .instructions()
        .iter()
        .map(|instruction| compile_instruction(instruction, global_variables, type_ids))
        .chain(vec![compile_terminal_instruction(
            block.terminal_instruction(),
            branch_variable_name,
            global_variables,
            type_ids,
        )])
        .map(|string| "  ".to_owned() + &string)
        .collect::<Vec<_>>()
        .join("\n")
}

fn compile_instruction(
    instruction: &Instruction,
    global_variables: &HashSet<String>,
    type_ids: &HashMap<fmm::types::Type, String>,
) -> String {
    let compile_expression =
        |expression| compile_expression(expression, global_variables, type_ids);
    let compile_typed_name = |type_, name| compile_typed_name(type_, name, type_ids);
    let compile_type_id = |type_| compile_type_id(type_, type_ids);

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
        Instruction::AllocateStack(allocate) => {
            let entity_name = allocate.name().to_owned() + "_entity";

            format!(
                "{};\n  {}=&{};",
                compile_typed_name(allocate.type_(), &entity_name),
                compile_typed_name(
                    &types::Pointer::new(allocate.type_().clone()).into(),
                    allocate.name(),
                ),
                entity_name,
            )
        }
        Instruction::ArithmeticOperation(operation) => format!(
            "{}={}{}{};",
            compile_typed_name(&operation.type_().into(), operation.name()),
            compile_expression(operation.lhs()),
            compile_arithmetic_operator(operation.operator()),
            compile_expression(operation.rhs()),
        ),
        Instruction::AtomicLoad(load) => format!(
            "{}=({})atomic_load(({}){});",
            compile_typed_name(&load.type_(), load.name()),
            compile_type_id(load.type_()),
            compile_atomic_pointer_type_id(load.type_(), type_ids),
            compile_expression(load.pointer()),
        ),
        Instruction::AtomicStore(store) => format!(
            "atomic_store(({}){},{});",
            compile_atomic_pointer_type_id(store.type_(), type_ids),
            compile_expression(store.pointer()),
            compile_expression(store.value()),
        ),
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
        Instruction::CompareAndSwap(cas) => {
            let name = "_cas_".to_owned() + cas.name();

            format!(
                "{}={};\n  bool {}=atomic_compare_exchange_strong(({}){},&{},{});",
                compile_typed_name(cas.type_(), &name),
                compile_expression(cas.old_value()),
                cas.name(),
                compile_atomic_pointer_type_id(cas.type_(), type_ids),
                compile_expression(cas.pointer()),
                name,
                compile_expression(cas.new_value()),
            )
        }
        Instruction::ComparisonOperation(operation) => format!(
            "bool {}={}{}{};",
            operation.name(),
            compile_expression(operation.lhs()),
            compile_comparison_operator(operation.operator()),
            compile_expression(operation.rhs()),
        ),
        Instruction::DeconstructRecord(deconstruct) => format!(
            "{}={}.{};",
            compile_typed_name(
                &deconstruct.type_().elements()[deconstruct.element_index()],
                deconstruct.name(),
            ),
            compile_expression(deconstruct.record()),
            generate_record_element_name(deconstruct.element_index()),
        ),
        Instruction::DeconstructUnion(deconstruct) => format!(
            "{}={}.{};",
            compile_typed_name(
                &deconstruct.type_().members()[deconstruct.member_index()],
                deconstruct.name(),
            ),
            compile_expression(deconstruct.union()),
            generate_union_member_name(deconstruct.member_index()),
        ),
        Instruction::If(if_) => {
            let compile_block =
                |block| compile_block(block, Some(if_.name()), global_variables, type_ids);

            format!(
                "{};if({}){{\n{}\n  }}else{{\n{}\n  }}",
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
        Instruction::PointerAddress(address) => format!(
            "{}={}+{};",
            compile_typed_name(&address.type_().clone().into(), address.name()),
            compile_expression(address.pointer()),
            compile_expression(address.offset()),
        ),
        Instruction::RecordAddress(address) => format!(
            "{}=&({})->{};",
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
                "{}=&({})->{};",
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
    global_variables: &HashSet<String>,
    type_ids: &HashMap<fmm::types::Type, String>,
) -> String {
    let compile_expression =
        |expression| compile_expression(expression, global_variables, type_ids);

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

fn compile_arithmetic_operator(operator: ArithmeticOperator) -> &'static str {
    match operator {
        ArithmeticOperator::Add => "+",
        ArithmeticOperator::Subtract => "-",
        ArithmeticOperator::Multiply => "*",
        ArithmeticOperator::Divide => "/",
    }
}

fn compile_comparison_operator(operator: ComparisonOperator) -> &'static str {
    match operator {
        ComparisonOperator::Equal => "==",
        ComparisonOperator::NotEqual => "!=",
        ComparisonOperator::LessThan => "<",
        ComparisonOperator::LessThanOrEqual => "<=",
        ComparisonOperator::GreaterThan => ">",
        ComparisonOperator::GreaterThanOrEqual => ">=",
    }
}
