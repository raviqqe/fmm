use crate::{ir::*, types};
use indexmap::IndexSet;
use std::mem::swap;

pub fn transform(module: &mut Module) {
    for definition in module.function_definitions_mut() {
        transform_function_definition(definition);
    }
}

fn transform_function_definition(definition: &mut FunctionDefinition) {
    if definition.type_().calling_convention() != types::CallingConvention::Source {
        return;
    }

    transform_block(definition.body_mut(), &mut IndexSet::default());
}

fn transform_block(block: &mut Block, variables: &mut IndexSet<String>) {
    let (instructions, terminal_instruction) = block.parts_mut();

    collect_from_instructions(instructions, terminal_instruction, variables);
}

pub fn collect(instructions: &mut [Instruction], terminal_instruction: &TerminalInstruction) {
    let mut variables = IndexSet::default();

    collect_from_instructions(instructions, terminal_instruction, &mut variables);
}

fn collect_from_instructions(
    instructions: &mut [Instruction],
    terminal_instruction: &TerminalInstruction,
    variables: &mut IndexSet<String>,
) {
    collect_from_terminal_instruction(terminal_instruction, variables);

    for instruction in instructions.iter_mut().rev() {
        if let Some((name, _)) = instruction.value() {
            variables.remove(name);
        }

        collect_from_instruction(instruction, variables);
    }
}

fn collect_from_instruction(instruction: &mut Instruction, variables: &mut IndexSet<String>) {
    let mut collect = |expression| collect_from_expression(expression, variables);

    match instruction {
        Instruction::AllocateHeap(allocate) => collect(allocate.size()),
        Instruction::AtomicLoad(load) => collect(load.pointer()),
        Instruction::AtomicOperation(operation) => {
            collect(operation.pointer());
            collect(operation.value());
        }
        Instruction::AtomicStore(store) => {
            collect(store.value());
            collect(store.pointer());
        }
        Instruction::Call(call) => {
            if call.type_().calling_convention() == types::CallingConvention::Source {
                *call.environment_mut() = Some(variables.clone());
            }

            collect_from_expression(call.function(), variables);

            for argument in call.arguments() {
                collect_from_expression(argument, variables);
            }
        }
        Instruction::CompareAndSwap(cas) => {
            collect(cas.pointer());
            collect(cas.old_value());
            collect(cas.new_value());
        }
        Instruction::DeconstructRecord(deconstruct) => collect(deconstruct.record()),
        Instruction::DeconstructUnion(deconstruct) => collect(deconstruct.union()),
        Instruction::FreeHeap(free) => collect(free.pointer()),
        Instruction::If(if_) => {
            // TODO Optimize this clone.
            let mut other_variables = variables.clone();

            transform_block(if_.then_mut(), variables);
            transform_block(if_.else_mut(), &mut other_variables);

            // Choose a longer match.
            if variables.len() < other_variables.len() {
                swap(variables, &mut other_variables);
            }

            variables.union(&other_variables);

            collect_from_expression(if_.condition(), variables);
        }
        Instruction::Load(load) => collect(load.pointer()),
        Instruction::MemoryCopy(copy) => {
            collect(copy.source());
            collect(copy.destination());
            collect(copy.size());
        }
        Instruction::ReallocateHeap(reallocate) => {
            collect(reallocate.pointer());
            collect(reallocate.size());
        }
        Instruction::Store(store) => {
            collect(store.value());
            collect(store.pointer());
        }
        Instruction::Fence(_) | Instruction::AllocateStack(_) => Default::default(),
    }
}

fn collect_from_terminal_instruction(
    instruction: &TerminalInstruction,
    variables: &mut IndexSet<String>,
) {
    match instruction {
        TerminalInstruction::Branch(branch) => {
            collect_from_expression(branch.expression(), variables)
        }
        TerminalInstruction::Return(return_) => {
            variables.clear();
            collect_from_expression(return_.expression(), variables)
        }
        TerminalInstruction::Unreachable => {
            variables.clear();
        }
    }
}

fn collect_from_expression(expression: &Expression, variables: &mut IndexSet<String>) {
    let mut collect = |expression| collect_from_expression(expression, variables);

    match expression {
        Expression::ArithmeticOperation(operation) => {
            collect(operation.lhs());
            collect(operation.rhs());
        }
        Expression::BitCast(bit_cast) => collect(bit_cast.expression()),
        Expression::BitwiseNotOperation(operation) => collect(operation.value()),
        Expression::BitwiseOperation(operation) => {
            collect(operation.lhs());
            collect(operation.rhs());
        }
        Expression::ComparisonOperation(operation) => {
            collect(operation.lhs());
            collect(operation.rhs());
        }
        Expression::PointerAddress(address) => {
            collect(address.pointer());
            collect(address.offset());
        }
        Expression::Record(record) => {
            for field in record.fields() {
                collect(field);
            }
        }
        Expression::RecordAddress(address) => collect(address.pointer()),
        Expression::Union(union) => collect(union.member()),
        Expression::UnionAddress(address) => collect(address.pointer()),
        Expression::Variable(variable) => {
            variables.insert(variable.name().into());
        }
        Expression::AlignOf(_)
        | Expression::Primitive(_)
        | Expression::SizeOf(_)
        | Expression::Undefined(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{analysis::validation, types};
    use indexmap::IndexSet;
    use pretty_assertions::assert_eq;

    fn transform_module(mut module: Module) -> Module {
        validation::validate(&module).unwrap();
        transform(&mut module);
        validation::validate(&module).unwrap();

        module
    }

    #[test]
    fn transform_empty_definition() {
        let module = Module::new(
            vec![],
            vec![],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![],
                types::Primitive::PointerInteger,
                Block::new(vec![], TerminalInstruction::Unreachable),
                Default::default(),
            )],
        );

        assert_eq!(transform_module(module.clone()), module);
    }

    #[test]
    fn collect_no_free_variable_in_call() {
        let function_type = types::Function::new(
            vec![],
            types::Primitive::PointerInteger,
            types::CallingConvention::Source,
        );
        let mut instructions =
            vec![Call::new(function_type.clone(), Variable::new("f"), vec![], "x").into()];
        let mut terminal_instruction =
            Return::new(types::Primitive::PointerInteger, Variable::new("x")).into();

        collect(&mut instructions, &mut terminal_instruction);

        assert_eq!(
            instructions,
            &[{
                let mut call = Call::new(function_type, Variable::new("f"), vec![], "x");

                *call.environment_mut() = Some(IndexSet::default());

                call
            }
            .into()]
        );
    }

    #[test]
    fn collect_free_variable_from_terminal_instruction() {
        let function_type = types::Function::new(
            vec![],
            types::Primitive::PointerInteger,
            types::CallingConvention::Source,
        );
        let mut instructions =
            vec![Call::new(function_type.clone(), Variable::new("f"), vec![], "x").into()];
        let mut terminal_instruction =
            Return::new(types::Primitive::PointerInteger, Variable::new("y")).into();

        collect(&mut instructions, &mut terminal_instruction);

        assert_eq!(
            instructions,
            &[{
                let mut call = Call::new(function_type, Variable::new("f"), vec![], "x");

                *call.environment_mut() = Some(IndexSet::<String>::from_iter(["y".into()]));

                call
            }
            .into()]
        );
    }

    mod if_ {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn collect_free_variables_in_then_block() {
            let function_type = types::Function::new(
                vec![],
                types::Primitive::PointerInteger,
                types::CallingConvention::Source,
            );
            let function_declarations = vec![FunctionDeclaration::new("g", function_type.clone())];

            assert_eq!(
                transform_module(Module::new(
                    vec![],
                    function_declarations.clone(),
                    vec![],
                    vec![FunctionDefinition::new(
                        "f",
                        vec![
                            Argument::new(
                                "p",
                                types::Pointer::new(types::Primitive::PointerInteger)
                            ),
                            Argument::new(
                                "q",
                                types::Pointer::new(types::Primitive::PointerInteger)
                            ),
                            Argument::new("r", types::Primitive::PointerInteger)
                        ],
                        types::Primitive::PointerInteger,
                        Block::new(
                            vec![
                                If::new(
                                    types::Primitive::PointerInteger,
                                    Primitive::Boolean(true),
                                    Block::new(vec![], TerminalInstruction::Unreachable),
                                    Block::new(
                                        vec![
                                            Call::new(
                                                function_type.clone(),
                                                Variable::new("g"),
                                                vec![],
                                                "i",
                                            )
                                            .into(),
                                            Load::new(
                                                types::Primitive::PointerInteger,
                                                Variable::new("p"),
                                                "j"
                                            )
                                            .into(),
                                        ],
                                        Branch::new(
                                            types::Primitive::PointerInteger,
                                            Primitive::PointerInteger(0),
                                        ),
                                    ),
                                    "k",
                                )
                                .into(),
                                Load::new(
                                    types::Primitive::PointerInteger,
                                    Variable::new("q"),
                                    "l"
                                )
                                .into(),
                            ],
                            Return::new(types::Primitive::PointerInteger, Variable::new("r")),
                        ),
                        Default::default(),
                    )],
                )),
                Module::new(
                    vec![],
                    function_declarations.clone(),
                    vec![],
                    vec![FunctionDefinition::new(
                        "f",
                        vec![
                            Argument::new(
                                "p",
                                types::Pointer::new(types::Primitive::PointerInteger)
                            ),
                            Argument::new(
                                "q",
                                types::Pointer::new(types::Primitive::PointerInteger)
                            ),
                            Argument::new("r", types::Primitive::PointerInteger)
                        ],
                        types::Primitive::PointerInteger,
                        Block::new(
                            vec![
                                If::new(
                                    types::Primitive::PointerInteger,
                                    Primitive::Boolean(true),
                                    Block::new(vec![], TerminalInstruction::Unreachable),
                                    Block::new(
                                        vec![
                                            {
                                                let mut call = Call::new(
                                                    function_type,
                                                    Variable::new("g"),
                                                    vec![],
                                                    "i",
                                                );

                                                *call.environment_mut() =
                                                    Some(IndexSet::<String>::from_iter([
                                                        "r".into(),
                                                        "q".into(),
                                                        "p".into(),
                                                    ]));

                                                call
                                            }
                                            .into(),
                                            Load::new(
                                                types::Primitive::PointerInteger,
                                                Variable::new("p"),
                                                "j"
                                            )
                                            .into(),
                                        ],
                                        Branch::new(
                                            types::Primitive::PointerInteger,
                                            Primitive::PointerInteger(0),
                                        ),
                                    ),
                                    "k",
                                )
                                .into(),
                                Load::new(
                                    types::Primitive::PointerInteger,
                                    Variable::new("q"),
                                    "l"
                                )
                                .into(),
                            ],
                            Return::new(types::Primitive::PointerInteger, Variable::new("r")),
                        ),
                        Default::default(),
                    )],
                )
            );
        }

        #[test]
        fn collect_free_variables_in_else_block() {
            let function_type = types::Function::new(
                vec![],
                types::Primitive::PointerInteger,
                types::CallingConvention::Source,
            );
            let function_declarations = vec![FunctionDeclaration::new("g", function_type.clone())];

            assert_eq!(
                transform_module(Module::new(
                    vec![],
                    function_declarations.clone(),
                    vec![],
                    vec![FunctionDefinition::new(
                        "f",
                        vec![
                            Argument::new(
                                "p",
                                types::Pointer::new(types::Primitive::PointerInteger)
                            ),
                            Argument::new(
                                "q",
                                types::Pointer::new(types::Primitive::PointerInteger)
                            ),
                            Argument::new("r", types::Primitive::PointerInteger)
                        ],
                        types::Primitive::PointerInteger,
                        Block::new(
                            vec![
                                If::new(
                                    types::Primitive::PointerInteger,
                                    Primitive::Boolean(true),
                                    Block::new(
                                        vec![
                                            Call::new(
                                                function_type.clone(),
                                                Variable::new("g"),
                                                vec![],
                                                "i",
                                            )
                                            .into(),
                                            Load::new(
                                                types::Primitive::PointerInteger,
                                                Variable::new("p"),
                                                "j"
                                            )
                                            .into(),
                                        ],
                                        Branch::new(
                                            types::Primitive::PointerInteger,
                                            Primitive::PointerInteger(0),
                                        ),
                                    ),
                                    Block::new(vec![], TerminalInstruction::Unreachable),
                                    "k",
                                )
                                .into(),
                                Load::new(
                                    types::Primitive::PointerInteger,
                                    Variable::new("q"),
                                    "l"
                                )
                                .into(),
                            ],
                            Return::new(types::Primitive::PointerInteger, Variable::new("r")),
                        ),
                        Default::default(),
                    )],
                )),
                Module::new(
                    vec![],
                    function_declarations.clone(),
                    vec![],
                    vec![FunctionDefinition::new(
                        "f",
                        vec![
                            Argument::new(
                                "p",
                                types::Pointer::new(types::Primitive::PointerInteger)
                            ),
                            Argument::new(
                                "q",
                                types::Pointer::new(types::Primitive::PointerInteger)
                            ),
                            Argument::new("r", types::Primitive::PointerInteger)
                        ],
                        types::Primitive::PointerInteger,
                        Block::new(
                            vec![
                                If::new(
                                    types::Primitive::PointerInteger,
                                    Primitive::Boolean(true),
                                    Block::new(
                                        vec![
                                            {
                                                let mut call = Call::new(
                                                    function_type,
                                                    Variable::new("g"),
                                                    vec![],
                                                    "i",
                                                );

                                                *call.environment_mut() =
                                                    Some(IndexSet::<String>::from_iter([
                                                        "r".into(),
                                                        "q".into(),
                                                        "p".into(),
                                                    ]));

                                                call
                                            }
                                            .into(),
                                            Load::new(
                                                types::Primitive::PointerInteger,
                                                Variable::new("p"),
                                                "j"
                                            )
                                            .into(),
                                        ],
                                        Branch::new(
                                            types::Primitive::PointerInteger,
                                            Primitive::PointerInteger(0),
                                        ),
                                    ),
                                    Block::new(vec![], TerminalInstruction::Unreachable),
                                    "k",
                                )
                                .into(),
                                Load::new(
                                    types::Primitive::PointerInteger,
                                    Variable::new("q"),
                                    "l"
                                )
                                .into(),
                            ],
                            Return::new(types::Primitive::PointerInteger, Variable::new("r")),
                        ),
                        Default::default(),
                    )],
                )
            );
        }

        #[test]
        fn collect_different_free_variables_in_then_and_else_blocks() {
            let function_type = types::Function::new(
                vec![],
                types::Primitive::PointerInteger,
                types::CallingConvention::Source,
            );
            let function_declarations = vec![FunctionDeclaration::new("g", function_type.clone())];

            assert_eq!(
                transform_module(Module::new(
                    vec![],
                    function_declarations.clone(),
                    vec![],
                    vec![FunctionDefinition::new(
                        "f",
                        vec![
                            Argument::new(
                                "p1",
                                types::Pointer::new(types::Primitive::PointerInteger)
                            ),
                            Argument::new(
                                "p2",
                                types::Pointer::new(types::Primitive::PointerInteger)
                            ),
                            Argument::new(
                                "q",
                                types::Pointer::new(types::Primitive::PointerInteger)
                            ),
                            Argument::new("r", types::Primitive::PointerInteger)
                        ],
                        types::Primitive::PointerInteger,
                        Block::new(
                            vec![
                                If::new(
                                    types::Primitive::PointerInteger,
                                    Primitive::Boolean(true),
                                    Block::new(
                                        vec![
                                            Call::new(
                                                function_type.clone(),
                                                Variable::new("g"),
                                                vec![],
                                                "i1",
                                            )
                                            .into(),
                                            Load::new(
                                                types::Primitive::PointerInteger,
                                                Variable::new("p1"),
                                                "j1"
                                            )
                                            .into(),
                                        ],
                                        Branch::new(
                                            types::Primitive::PointerInteger,
                                            Primitive::PointerInteger(0),
                                        ),
                                    ),
                                    Block::new(
                                        vec![
                                            Call::new(
                                                function_type.clone(),
                                                Variable::new("g"),
                                                vec![],
                                                "i2",
                                            )
                                            .into(),
                                            Load::new(
                                                types::Primitive::PointerInteger,
                                                Variable::new("p2"),
                                                "j2"
                                            )
                                            .into(),
                                        ],
                                        Branch::new(
                                            types::Primitive::PointerInteger,
                                            Primitive::PointerInteger(0),
                                        ),
                                    ),
                                    "k",
                                )
                                .into(),
                                Load::new(
                                    types::Primitive::PointerInteger,
                                    Variable::new("q"),
                                    "l"
                                )
                                .into(),
                            ],
                            Return::new(types::Primitive::PointerInteger, Variable::new("r")),
                        ),
                        Default::default(),
                    )],
                )),
                Module::new(
                    vec![],
                    function_declarations.clone(),
                    vec![],
                    vec![FunctionDefinition::new(
                        "f",
                        vec![
                            Argument::new(
                                "p1",
                                types::Pointer::new(types::Primitive::PointerInteger)
                            ),
                            Argument::new(
                                "p2",
                                types::Pointer::new(types::Primitive::PointerInteger)
                            ),
                            Argument::new(
                                "q",
                                types::Pointer::new(types::Primitive::PointerInteger)
                            ),
                            Argument::new("r", types::Primitive::PointerInteger)
                        ],
                        types::Primitive::PointerInteger,
                        Block::new(
                            vec![
                                If::new(
                                    types::Primitive::PointerInteger,
                                    Primitive::Boolean(true),
                                    Block::new(
                                        vec![
                                            {
                                                let mut call = Call::new(
                                                    function_type.clone(),
                                                    Variable::new("g"),
                                                    vec![],
                                                    "i1",
                                                );

                                                *call.environment_mut() =
                                                    Some(IndexSet::<String>::from_iter([
                                                        "r".into(),
                                                        "q".into(),
                                                        "p1".into(),
                                                    ]));

                                                call
                                            }
                                            .into(),
                                            Load::new(
                                                types::Primitive::PointerInteger,
                                                Variable::new("p1"),
                                                "j1"
                                            )
                                            .into(),
                                        ],
                                        Branch::new(
                                            types::Primitive::PointerInteger,
                                            Primitive::PointerInteger(0),
                                        ),
                                    ),
                                    Block::new(
                                        vec![
                                            {
                                                let mut call = Call::new(
                                                    function_type,
                                                    Variable::new("g"),
                                                    vec![],
                                                    "i2",
                                                );

                                                *call.environment_mut() =
                                                    Some(IndexSet::<String>::from_iter([
                                                        "r".into(),
                                                        "q".into(),
                                                        "p2".into(),
                                                    ]));

                                                call
                                            }
                                            .into(),
                                            Load::new(
                                                types::Primitive::PointerInteger,
                                                Variable::new("p2"),
                                                "j2"
                                            )
                                            .into(),
                                        ],
                                        Branch::new(
                                            types::Primitive::PointerInteger,
                                            Primitive::PointerInteger(0),
                                        ),
                                    ),
                                    "k",
                                )
                                .into(),
                                Load::new(
                                    types::Primitive::PointerInteger,
                                    Variable::new("q"),
                                    "l"
                                )
                                .into(),
                            ],
                            Return::new(types::Primitive::PointerInteger, Variable::new("r")),
                        ),
                        Default::default(),
                    )],
                )
            );
        }
    }
}
