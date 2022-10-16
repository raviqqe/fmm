use crate::{ir::*, types::Type};
use fnv::FnvHashMap;

pub fn collect(definition: &FunctionDefinition) -> FnvHashMap<&str, Type> {
    let mut variables = definition
        .arguments()
        .iter()
        .map(|argument| (argument.name(), argument.type_().clone()))
        .collect();

    collect_from_block(definition.body(), &mut variables);

    variables
}

fn collect_from_block<'a>(block: &'a Block, variables: &mut FnvHashMap<&'a str, Type>) {
    variables.extend(
        block
            .instructions()
            .iter()
            .flat_map(|instruction| instruction.value()),
    );

    for instruction in block.instructions() {
        if let Instruction::If(if_) = instruction {
            collect_from_block(if_.then(), variables);
            collect_from_block(if_.else_(), variables);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types;
    use pretty_assertions::assert_eq;

    #[test]
    fn collect_function() {
        assert_eq!(
            collect(&FunctionDefinition::new(
                "f",
                vec![],
                types::Primitive::PointerInteger,
                Block::new(vec![], TerminalInstruction::Unreachable),
                Default::default()
            )),
            Default::default()
        );
    }

    #[test]
    fn collect_argument() {
        assert_eq!(
            collect(&FunctionDefinition::new(
                "f",
                vec![Argument::new("x", types::Primitive::PointerInteger)],
                types::Primitive::PointerInteger,
                Block::new(vec![], TerminalInstruction::Unreachable),
                Default::default()
            )),
            [("x", types::Primitive::PointerInteger.into())]
                .into_iter()
                .collect()
        );
    }

    #[test]
    fn collect_instruction_value() {
        assert_eq!(
            collect(&FunctionDefinition::new(
                "f",
                vec![],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![Call::new(
                        types::Function::new(
                            vec![],
                            types::Primitive::PointerInteger,
                            types::CallingConvention::Source
                        ),
                        Variable::new("f"),
                        vec![],
                        "x"
                    )
                    .into()],
                    TerminalInstruction::Unreachable
                ),
                Default::default()
            )),
            [("x", types::Primitive::PointerInteger.into())]
                .into_iter()
                .collect()
        );
    }

    #[test]
    fn collect_instruction_value_in_nested_block() {
        assert_eq!(
            collect(&FunctionDefinition::new(
                "f",
                vec![],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![If::new(
                        types::Primitive::PointerInteger,
                        Primitive::Boolean(true),
                        Block::new(
                            vec![Call::new(
                                types::Function::new(
                                    vec![],
                                    types::Primitive::PointerInteger,
                                    types::CallingConvention::Source
                                ),
                                Variable::new("f"),
                                vec![],
                                "x"
                            )
                            .into()],
                            TerminalInstruction::Unreachable
                        ),
                        Block::new(
                            vec![Call::new(
                                types::Function::new(
                                    vec![],
                                    types::Primitive::PointerInteger,
                                    types::CallingConvention::Source
                                ),
                                Variable::new("f"),
                                vec![],
                                "y"
                            )
                            .into()],
                            TerminalInstruction::Unreachable
                        ),
                        "z"
                    )
                    .into()],
                    TerminalInstruction::Unreachable
                ),
                Default::default()
            )),
            [
                ("x", types::Primitive::PointerInteger.into()),
                ("y", types::Primitive::PointerInteger.into()),
                ("z", types::Primitive::PointerInteger.into())
            ]
            .into_iter()
            .collect()
        );
    }
}
