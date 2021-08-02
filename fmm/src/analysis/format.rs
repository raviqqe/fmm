use crate::ir::*;

pub fn format(module: &Module) -> String {
    format!(
        "(module\n{}\n)",
        module
            .function_definitions()
            .iter()
            .map(|definition| format_function_definition(definition, 1))
            .collect::<Vec<_>>()
            .join("\n")
    )
}

fn format_function_definition(definition: &FunctionDefinition, level: usize) -> String {
    format!(
        "{indent}(function {} {}\n{}\n{indent})",
        definition.name(),
        definition
            .arguments()
            .iter()
            .map(|argument| argument.name())
            .collect::<Vec<_>>()
            .join(" "),
        format_block(definition.body(), level + 1),
        indent = indent(level)
    )
}

fn format_block(block: &Block, level: usize) -> String {
    let instructions = block
        .instructions()
        .iter()
        .map(|instruction| format_instruction(instruction, level + 1))
        .collect::<Vec<_>>()
        .join(" ");

    format!(
        "{indent}(\n{}{}\n{indent})",
        if instructions.is_empty() {
            "".into()
        } else {
            instructions.to_owned() + "\n"
        },
        indent(level + 1) + &format_terminal_instruction(block.terminal_instruction()),
        indent = indent(level)
    )
}

fn format_instruction(instruction: &Instruction, level: usize) -> String {
    indent(level)
        + &match instruction {
            Instruction::Call(call) => format!(
                "(call {} {})",
                format_expression(call.function()),
                call.arguments()
                    .iter()
                    .map(|argument| format_expression(argument))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Instruction::If(if_) => format!(
                "(if {}\n{}\n{})",
                format_expression(if_.condition()),
                format_block(if_.then(), level + 1),
                format_block(if_.else_(), level + 1),
            ),
            _ => todo!(),
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
        Expression::AlignOf(_) => "(align-of)".into(),
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
        Expression::Primitive(primitive) => format_primitive(primitive),
        Expression::SizeOf(_) => "(size-of)".into(),
        Expression::Undefined(_) => "undefined".into(),
        Expression::Variable(variable) => variable.name().into(),
        _ => todo!(),
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

fn indent(level: usize) -> String {
    "  ".repeat(level)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types;

    #[test]
    fn format_module_with_call() {
        insta::assert_snapshot!(
            "{}",
            format(&Module::new(
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
            ))
        );
    }

    #[test]
    fn format_module_with_if() {
        insta::assert_snapshot!(format(&Module::new(
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
}
