use crate::{ir::*, types};
use fnv::FnvHashSet;
use indexmap::IndexSet;
use std::{mem::swap, rc::Rc};

struct Context {
    global_variables: FnvHashSet<String>,
}

pub fn transform(module: &mut Module) {
    let context = Context {
        global_variables: module
            .variable_declarations()
            .iter()
            .map(|declaration| declaration.name())
            .chain(
                module
                    .function_declarations()
                    .iter()
                    .map(|declaration| declaration.name()),
            )
            .chain(
                module
                    .variable_definitions()
                    .iter()
                    .map(|definition| definition.name()),
            )
            .chain(
                module
                    .function_definitions()
                    .iter()
                    .map(|definition| definition.name()),
            )
            .map(|string| string.to_owned())
            .collect(),
    };

    for definition in module.function_definitions_mut() {
        transform_function_definition(&context, definition);
    }
}

fn transform_function_definition(context: &Context, definition: &mut FunctionDefinition) {
    if definition.type_().calling_convention() != types::CallingConvention::Source {
        return;
    }

    transform_block(context, definition.body_mut(), &mut IndexSet::default());
}

fn transform_block(context: &Context, block: &mut Block, variables: &mut IndexSet<Rc<str>>) {
    collect_from_terminal_instruction(context, block.terminal_instruction_mut(), variables);

    for instruction in block.instructions_mut().iter_mut().rev() {
        if let Some((name, _)) = instruction.value() {
            variables.remove(name);
        }

        collect_from_instruction(context, instruction, variables);
    }
}

fn collect_from_instruction(
    context: &Context,
    instruction: &mut Instruction,
    variables: &mut IndexSet<Rc<str>>,
) {
    let mut collect = |expression| collect_from_expression(context, expression, variables);

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
                *call.environment_mut() = variables.clone();
            }

            collect_from_expression(context, call.function(), variables);

            for argument in call.arguments() {
                collect_from_expression(context, argument, variables);
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
            // TODO Consider including if instructions' results to omit them in if
            // flattening.
            *if_.environment_mut() = variables.clone();

            let mut other_variables = variables.clone();

            transform_block(context, if_.then_mut(), variables);
            transform_block(context, if_.else_mut(), &mut other_variables);

            // Choose a bigger one as a left-hand side for merge.
            if variables.len() < other_variables.len() {
                swap(variables, &mut other_variables);
            }

            variables.extend(other_variables);

            collect_from_expression(context, if_.condition(), variables);
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

fn contains_instructon_with_environment(block: &Block) -> bool {
    block
        .instructions()
        .iter()
        .any(|instruction| match instruction {
            Instruction::Call(call)
                if call.type_().calling_convention() == types::CallingConvention::Source =>
            {
                true
            }
            Instruction::If(_) => true,
            _ => false,
        })
}

fn collect_from_terminal_instruction(
    context: &Context,
    instruction: &TerminalInstruction,
    variables: &mut IndexSet<Rc<str>>,
) {
    match instruction {
        TerminalInstruction::Branch(branch) => {
            collect_from_expression(context, branch.expression(), variables)
        }
        TerminalInstruction::Return(return_) => {
            variables.clear();
            collect_from_expression(context, return_.expression(), variables)
        }
        TerminalInstruction::Unreachable => {
            variables.clear();
        }
    }
}

fn collect_from_expression(
    context: &Context,
    expression: &Expression,
    variables: &mut IndexSet<Rc<str>>,
) {
    let mut collect = |expression| collect_from_expression(context, expression, variables);

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
            if !context.global_variables.contains(variable.name()) {
                variables.insert(variable.name_rc().clone());
            }
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
    fn collect_no_free_variable() {
        let function_type = types::Function::new(
            vec![],
            types::Primitive::PointerInteger,
            types::CallingConvention::Source,
        );
        let module = Module::new(
            vec![],
            vec![FunctionDeclaration::new("g", function_type.clone())],
            vec![],
            vec![FunctionDefinition::new(
                "f",
                vec![],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![Call::new(function_type, Variable::new("g"), vec![], "x").into()],
                    Return::new(types::Primitive::PointerInteger, Variable::new("x")),
                ),
                Default::default(),
            )],
        );

        assert_eq!(transform_module(module.clone()), module);
    }

    #[test]
    fn collect_free_variable_from_instruction() {
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
                    vec![Argument::new("y", types::Primitive::PointerInteger)],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![
                            Call::new(function_type.clone(), Variable::new("g"), vec![], "x")
                                .into(),
                            Store::new(
                                types::Primitive::PointerInteger,
                                Variable::new("y"),
                                Undefined::new(types::Pointer::new(
                                    types::Primitive::PointerInteger
                                )),
                            )
                            .into(),
                        ],
                        TerminalInstruction::Unreachable
                    ),
                    Default::default(),
                )],
            )),
            Module::new(
                vec![],
                function_declarations,
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![Argument::new("y", types::Primitive::PointerInteger)],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![
                            {
                                let mut call =
                                    Call::new(function_type, Variable::new("g"), vec![], "x");

                                *call.environment_mut() = IndexSet::from_iter(["y".into()]);

                                call
                            }
                            .into(),
                            Store::new(
                                types::Primitive::PointerInteger,
                                Variable::new("y"),
                                Undefined::new(types::Pointer::new(
                                    types::Primitive::PointerInteger
                                )),
                            )
                            .into()
                        ],
                        TerminalInstruction::Unreachable
                    ),
                    Default::default(),
                )],
            )
        );
    }

    #[test]
    fn collect_free_variable_from_terminal_instruction() {
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
                    vec![Argument::new("y", types::Primitive::PointerInteger)],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![
                            Call::new(function_type.clone(), Variable::new("g"), vec![], "x")
                                .into()
                        ],
                        Return::new(types::Primitive::PointerInteger, Variable::new("y"))
                    ),
                    Default::default(),
                )],
            )),
            Module::new(
                vec![],
                function_declarations,
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![Argument::new("y", types::Primitive::PointerInteger)],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![{
                            let mut call =
                                Call::new(function_type, Variable::new("g"), vec![], "x");

                            *call.environment_mut() = IndexSet::from_iter(["y".into()]);

                            call
                        }
                        .into()],
                        Return::new(types::Primitive::PointerInteger, Variable::new("y"))
                    ),
                    Default::default(),
                )],
            )
        );
    }

    #[test]
    fn collect_free_variable_from_instructions_in_order() {
        let function_type = types::Function::new(
            vec![],
            types::Primitive::PointerInteger,
            types::CallingConvention::Source,
        );
        let function_declarations = vec![FunctionDeclaration::new("g", function_type.clone())];
        let arguments = vec![
            Argument::new("y", types::Primitive::PointerInteger),
            Argument::new("z", types::Primitive::PointerInteger),
            Argument::new("v", types::Primitive::PointerInteger),
        ];
        let instructions = [
            Store::new(
                types::Primitive::PointerInteger,
                Variable::new("y"),
                Undefined::new(types::Pointer::new(types::Primitive::PointerInteger)),
            )
            .into(),
            Store::new(
                types::Primitive::PointerInteger,
                Variable::new("z"),
                Undefined::new(types::Pointer::new(types::Primitive::PointerInteger)),
            )
            .into(),
        ];

        assert_eq!(
            transform_module(Module::new(
                vec![],
                function_declarations.clone(),
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    arguments.clone(),
                    types::Primitive::PointerInteger,
                    Block::new(
                        [
                            Call::new(function_type.clone(), Variable::new("g"), vec![], "x")
                                .into(),
                        ]
                        .into_iter()
                        .chain(instructions.clone())
                        .collect(),
                        Return::new(types::Primitive::PointerInteger, Variable::new("v"))
                    ),
                    Default::default(),
                )],
            )),
            Module::new(
                vec![],
                function_declarations,
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    arguments,
                    types::Primitive::PointerInteger,
                    Block::new(
                        [{
                            let mut call =
                                Call::new(function_type, Variable::new("g"), vec![], "x");

                            *call.environment_mut() =
                                IndexSet::from_iter(["v".into(), "z".into(), "y".into()]);

                            call
                        }
                        .into(),]
                        .into_iter()
                        .chain(instructions)
                        .collect(),
                        Return::new(types::Primitive::PointerInteger, Variable::new("v"))
                    ),
                    Default::default(),
                )],
            )
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
                    function_declarations,
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
                                {
                                    let mut if_ = If::new(
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
                                                        IndexSet::from_iter([
                                                            "r".into(),
                                                            "q".into(),
                                                            "p".into(),
                                                        ]);

                                                    call
                                                }
                                                .into(),
                                                Load::new(
                                                    types::Primitive::PointerInteger,
                                                    Variable::new("p"),
                                                    "j",
                                                )
                                                .into(),
                                            ],
                                            Branch::new(
                                                types::Primitive::PointerInteger,
                                                Primitive::PointerInteger(0),
                                            ),
                                        ),
                                        "k",
                                    );

                                    *if_.environment_mut() =
                                        IndexSet::from_iter(["r".into(), "q".into()]);

                                    if_
                                }
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
                    function_declarations,
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
                                {
                                    let mut if_ = If::new(
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
                                                        IndexSet::from_iter([
                                                            "r".into(),
                                                            "q".into(),
                                                            "p".into(),
                                                        ]);

                                                    call
                                                }
                                                .into(),
                                                Load::new(
                                                    types::Primitive::PointerInteger,
                                                    Variable::new("p"),
                                                    "j",
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
                                    );

                                    *if_.environment_mut() =
                                        IndexSet::from_iter(["r".into(), "q".into()]);

                                    if_
                                }
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
                    function_declarations,
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
                                {
                                    let mut if_ = If::new(
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
                                                        IndexSet::from_iter([
                                                            "r".into(),
                                                            "q".into(),
                                                            "p1".into(),
                                                        ]);

                                                    call
                                                }
                                                .into(),
                                                Load::new(
                                                    types::Primitive::PointerInteger,
                                                    Variable::new("p1"),
                                                    "j1",
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
                                                        IndexSet::from_iter([
                                                            "r".into(),
                                                            "q".into(),
                                                            "p2".into(),
                                                        ]);

                                                    call
                                                }
                                                .into(),
                                                Load::new(
                                                    types::Primitive::PointerInteger,
                                                    Variable::new("p2"),
                                                    "j2",
                                                )
                                                .into(),
                                            ],
                                            Branch::new(
                                                types::Primitive::PointerInteger,
                                                Primitive::PointerInteger(0),
                                            ),
                                        ),
                                        "k",
                                    );

                                    *if_.environment_mut() =
                                        IndexSet::from_iter(["r".into(), "q".into()]);

                                    if_
                                }
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
        fn merge_free_variables_from_then_and_else_blocks() {
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
                                Call::new(function_type.clone(), Variable::new("g"), vec![], "x")
                                    .into(),
                                If::new(
                                    types::Primitive::PointerInteger,
                                    Primitive::Boolean(true),
                                    Block::new(
                                        vec![Load::new(
                                            types::Primitive::PointerInteger,
                                            Variable::new("p"),
                                            "i"
                                        )
                                        .into()],
                                        Branch::new(
                                            types::Primitive::PointerInteger,
                                            Primitive::PointerInteger(0),
                                        ),
                                    ),
                                    Block::new(
                                        vec![Load::new(
                                            types::Primitive::PointerInteger,
                                            Variable::new("q"),
                                            "j"
                                        )
                                        .into()],
                                        Branch::new(
                                            types::Primitive::PointerInteger,
                                            Primitive::PointerInteger(0),
                                        ),
                                    ),
                                    "k",
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
                    function_declarations,
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
                                {
                                    let mut call =
                                        Call::new(function_type, Variable::new("g"), vec![], "x");

                                    *call.environment_mut() =
                                        IndexSet::from_iter(["r".into(), "q".into(), "p".into()]);

                                    call
                                }
                                .into(),
                                {
                                    let mut if_ = If::new(
                                        types::Primitive::PointerInteger,
                                        Primitive::Boolean(true),
                                        Block::new(
                                            vec![Load::new(
                                                types::Primitive::PointerInteger,
                                                Variable::new("p"),
                                                "i",
                                            )
                                            .into()],
                                            Branch::new(
                                                types::Primitive::PointerInteger,
                                                Primitive::PointerInteger(0),
                                            ),
                                        ),
                                        Block::new(
                                            vec![Load::new(
                                                types::Primitive::PointerInteger,
                                                Variable::new("q"),
                                                "j",
                                            )
                                            .into()],
                                            Branch::new(
                                                types::Primitive::PointerInteger,
                                                Primitive::PointerInteger(0),
                                            ),
                                        ),
                                        "k",
                                    );

                                    *if_.environment_mut() = IndexSet::from_iter(["r".into()]);

                                    if_
                                }
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
