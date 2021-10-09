use crate::{
    ir::*,
    types::{self, Type},
};

pub fn format_module(module: &Module) -> String {
    format!(
        "(module\n{})",
        module
            .function_definitions()
            .iter()
            .map(|definition| indent(&format_function_definition(definition)))
            .collect::<Vec<_>>()
            .join("\n")
    )
}

fn format_function_definition(definition: &FunctionDefinition) -> String {
    format!(
        "(function {} {}\n{})",
        definition.name(),
        definition
            .arguments()
            .iter()
            .map(|argument| argument.name())
            .collect::<Vec<_>>()
            .join(" "),
        indent(&format_block(definition.body())),
    )
}

fn format_block(block: &Block) -> String {
    let instructions = block
        .instructions()
        .iter()
        .map(|instruction| indent(&format_instruction(instruction)))
        .collect::<Vec<_>>()
        .join("\n");

    format!(
        "(block\n{}{})",
        if instructions.is_empty() {
            "".into()
        } else {
            instructions + "\n"
        },
        indent(&format_terminal_instruction(block.terminal_instruction())),
    )
}

fn format_instruction(instruction: &Instruction) -> String {
    match instruction {
        Instruction::AllocateHeap(allocate) => {
            format!(
                "(allocate-heap {} {})",
                format_expression(allocate.size()),
                allocate.name()
            )
        }
        Instruction::AllocateStack(allocate) => format!(
            "(allocate-stack {} {})",
            format_type(allocate.type_()),
            allocate.name()
        ),
        Instruction::AtomicLoad(load) => {
            format!(
                "(atomic-load {} {})",
                format_expression(load.pointer()),
                load.name()
            )
        }
        Instruction::AtomicOperation(operation) => {
            format!(
                "(atomic{} {} {} {})",
                match operation.operator() {
                    AtomicOperator::Add => "+",
                    AtomicOperator::Subtract => "-",
                },
                format_expression(operation.pointer()),
                format_expression(operation.value()),
                operation.name()
            )
        }
        Instruction::AtomicStore(store) => {
            format!(
                "(atomic-store {} {})",
                format_expression(store.value()),
                format_expression(store.pointer()),
            )
        }
        Instruction::Call(call) => format!(
            "(call {} {} {})",
            format_expression(call.function()),
            call.arguments()
                .iter()
                .map(format_expression)
                .collect::<Vec<_>>()
                .join(" "),
            call.name(),
        ),
        Instruction::CompareAndSwap(cas) => {
            format!(
                "(compare-and-swap {} {} {} {})",
                format_expression(cas.pointer()),
                format_expression(cas.old_value()),
                format_expression(cas.new_value()),
                cas.name(),
            )
        }
        Instruction::DeconstructRecord(deconstruct) => {
            format!(
                "(deconstruct-record {} {} {})",
                format_expression(deconstruct.record()),
                deconstruct.field_index(),
                deconstruct.name(),
            )
        }
        Instruction::DeconstructUnion(deconstruct) => {
            format!(
                "(deconstruct-union {} {} {})",
                format_expression(deconstruct.union()),
                deconstruct.member_index(),
                deconstruct.name(),
            )
        }
        Instruction::Fence(_) => "(fence)".into(),
        Instruction::FreeHeap(free) => {
            format!("(free-heap {})", format_expression(free.pointer()))
        }
        Instruction::If(if_) => format!(
            "(if {}\n{}\n{}\n{})",
            format_expression(if_.condition()),
            indent(&format_block(if_.then())),
            indent(&format_block(if_.else_())),
            indent(if_.name()),
        ),
        Instruction::Load(load) => {
            format!(
                "(load {} {})",
                format_expression(load.pointer()),
                load.name()
            )
        }
        Instruction::PassThrough(pass) => {
            format!(
                "(pass {} {})",
                format_expression(pass.expression()),
                pass.name()
            )
        }
        Instruction::ReallocateHeap(allocate) => {
            format!(
                "(reallocate-heap {} {} {})",
                format_expression(allocate.pointer()),
                format_expression(allocate.size()),
                allocate.name()
            )
        }
        Instruction::Store(store) => {
            format!(
                "(store {} {})",
                format_expression(store.value()),
                format_expression(store.pointer()),
            )
        }
    }
}

fn format_terminal_instruction(instruction: &TerminalInstruction) -> String {
    match instruction {
        TerminalInstruction::Branch(branch) => {
            format!("(branch {})", format_expression(branch.expression()))
        }
        TerminalInstruction::Return(return_) => {
            format!("(return {})", format_expression(return_.expression()))
        }
        TerminalInstruction::Unreachable => "(unreachable)".into(),
    }
}

fn format_expression(expression: &Expression) -> String {
    match expression {
        Expression::AlignOf(align_of) => format!("(align-of {})", format_type(align_of.type_())),
        Expression::ArithmeticOperation(operation) => format!(
            "({} {} {})",
            match operation.operator() {
                ArithmeticOperator::Add => "+",
                ArithmeticOperator::Subtract => "-",
                ArithmeticOperator::Multiply => "*",
                ArithmeticOperator::Divide => "/",
            },
            format_expression(operation.lhs()),
            format_expression(operation.rhs())
        ),
        Expression::BitCast(bit_cast) => {
            format!("(bit-cast {})", format_expression(bit_cast.expression()))
        }
        Expression::BitwiseNotOperation(not) => {
            format!("(bit! {})", format_expression(not.value()))
        }
        Expression::BitwiseOperation(operation) => {
            format!(
                "(bit{} {} {})",
                match operation.operator() {
                    BitwiseOperator::And => "&",
                    BitwiseOperator::Or => "|",
                    BitwiseOperator::Xor => "^",
                },
                format_expression(operation.lhs()),
                format_expression(operation.rhs()),
            )
        }
        Expression::ComparisonOperation(operation) => format!(
            "({} {} {})",
            match operation.operator() {
                ComparisonOperator::Equal => "==",
                ComparisonOperator::NotEqual => "!=",
                ComparisonOperator::LessThan => "<",
                ComparisonOperator::LessThanOrEqual => "<=",
                ComparisonOperator::GreaterThan => ">",
                ComparisonOperator::GreaterThanOrEqual => ">=",
            },
            format_expression(operation.lhs()),
            format_expression(operation.rhs())
        ),
        Expression::PointerAddress(address) => format!(
            "(pointer-address {} {})",
            format_expression(address.pointer()),
            format_expression(address.offset()),
        ),
        Expression::Primitive(primitive) => format_primitive(primitive),
        Expression::Record(record) => {
            let fields = record
                .fields()
                .iter()
                .map(format_expression)
                .collect::<Vec<_>>()
                .join(" ");

            format!(
                "(record{})",
                if fields.is_empty() {
                    "".into()
                } else {
                    " ".to_owned() + &fields
                }
            )
        }
        Expression::RecordAddress(address) => format!(
            "(record-address {} {})",
            format_expression(address.pointer()),
            address.field_index(),
        ),
        Expression::SizeOf(size_of) => format!("(size-of {})", format_type(size_of.type_())),
        Expression::Undefined(_) => "undefined".into(),
        Expression::Union(union) => format!(
            "(union {} {})",
            union.member_index(),
            format_expression(union.member())
        ),
        Expression::UnionAddress(address) => format!(
            "(union-address {} {})",
            format_expression(address.pointer()),
            address.member_index(),
        ),
        Expression::Variable(variable) => variable.name().into(),
    }
}

fn format_primitive(primitive: &Primitive) -> String {
    match primitive {
        Primitive::Boolean(boolean) => format!("{}", boolean),
        Primitive::Float32(number) => format!("{}", number),
        Primitive::Float64(number) => format!("{}", number),
        Primitive::Integer8(number) => format!("{}", number),
        Primitive::Integer32(number) => format!("{}", number),
        Primitive::Integer64(number) => format!("{}", number),
        Primitive::PointerInteger(number) => format!("{}", number),
    }
}

fn format_type(type_: &Type) -> String {
    match type_ {
        Type::Function(_) => "function".into(),
        Type::Primitive(primitive) => match primitive {
            types::Primitive::Boolean => "boolean",
            types::Primitive::Integer8 => "integer8",
            types::Primitive::Integer32 => "integer32",
            types::Primitive::Integer64 => "integer64",
            types::Primitive::Float32 => "float32",
            types::Primitive::Float64 => "float64",
            types::Primitive::PointerInteger => "pointer-integer",
        }
        .into(),
        Type::Pointer(pointer) => format!("(pointer {})", format_type(pointer.element())),
        Type::Record(_) => "record".into(),
        Type::Union(_) => "union".into(),
    }
}

fn indent(string: &str) -> String {
    regex::Regex::new("^|\n")
        .unwrap()
        .replace_all(string, "${0}  ")
        .into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types;

    #[test]
    fn format_module_with_call() {
        insta::assert_snapshot!(format_module(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "foo",
                vec![],
                Block::new(
                    vec![Call::new(
                        types::Function::new(
                            vec![],
                            types::Primitive::Boolean,
                            types::CallingConvention::Source
                        ),
                        Variable::new("f"),
                        vec![Variable::new("a1").into(), Variable::new("a2").into()],
                        "result",
                    )
                    .into()],
                    Return::new(types::Primitive::Boolean, Primitive::Boolean(true))
                ),
                types::Primitive::Boolean,
                types::CallingConvention::Source,
                Linkage::Internal
            )]
        )));
    }

    #[test]
    fn format_module_with_multiple_calls() {
        insta::assert_snapshot!(format_module(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "foo",
                vec![],
                Block::new(
                    vec![
                        Call::new(
                            types::Function::new(
                                vec![],
                                types::Primitive::Boolean,
                                types::CallingConvention::Source
                            ),
                            Variable::new("f"),
                            vec![Variable::new("a1").into(), Variable::new("a2").into()],
                            "result",
                        )
                        .into(),
                        Call::new(
                            types::Function::new(
                                vec![],
                                types::Primitive::Boolean,
                                types::CallingConvention::Source
                            ),
                            Variable::new("f"),
                            vec![Variable::new("a1").into(), Variable::new("a2").into()],
                            "result",
                        )
                        .into()
                    ],
                    Return::new(types::Primitive::Boolean, Primitive::Boolean(true))
                ),
                types::Primitive::Boolean,
                types::CallingConvention::Source,
                Linkage::Internal
            )]
        )));
    }

    #[test]
    fn format_module_with_if() {
        insta::assert_snapshot!(format_module(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![],
                Block::new(
                    vec![If::new(
                        types::Primitive::Float64,
                        Primitive::Boolean(true),
                        Block::new(
                            vec![],
                            Branch::new(types::Primitive::Float64, Primitive::Float64(42.0)),
                        ),
                        Block::new(
                            vec![],
                            Branch::new(types::Primitive::Float64, Primitive::Float64(42.0)),
                        ),
                        "x",
                    )
                    .into()],
                    Return::new(types::Primitive::Float64, Variable::new("x")),
                ),
                types::Primitive::Float64,
                types::CallingConvention::Source,
                Linkage::Internal
            )],
        )));
    }

    #[test]
    fn format_record_without_any_field() {
        assert_eq!(
            format_expression(&Record::new(types::Record::new(vec![]), vec![]).into()),
            "(record)"
        );
    }

    #[test]
    fn format_record_with_field() {
        assert_eq!(
            format_expression(
                &Record::new(
                    types::Record::new(vec![types::Primitive::Integer8.into()]),
                    vec![Primitive::Integer8(42).into()]
                )
                .into()
            ),
            "(record 42)"
        );
    }
}
