use super::free_variable;
use crate::{
    analysis::{expression_conversion, local_variable},
    build::NameGenerator,
    ir::*,
    types::{self, void_type, Type},
};
use fnv::FnvHashMap;

struct Context {
    function_definitions: Vec<FunctionDefinition>,
    name_generator: NameGenerator,
}

// TODO Consider integrating this logic deeply with CPS transformation to omit
// extra CPS stack manipulation.
pub fn flatten(module: &Module) -> Module {
    let mut context = Context {
        function_definitions: vec![],
        name_generator: NameGenerator::new("_if_"),
    };

    Module::new(
        module.variable_declarations().to_vec(),
        module.function_declarations().to_vec(),
        module.variable_definitions().to_vec(),
        module
            .function_definitions()
            .iter()
            .map(|definition| transform_function_definition(&mut context, definition))
            .collect::<Vec<_>>()
            .into_iter()
            .chain(context.function_definitions)
            .collect::<Vec<_>>(),
    )
}

fn transform_function_definition(
    context: &mut Context,
    definition: &FunctionDefinition,
) -> FunctionDefinition {
    if definition.type_().calling_convention() == types::CallingConvention::Source {
        FunctionDefinition::new(
            definition.name(),
            definition.arguments().to_vec(),
            definition.result_type().clone(),
            transform_block(
                context,
                definition.body(),
                definition.result_type(),
                &local_variable::collect(definition),
            ),
            definition.options().clone(),
        )
    } else {
        definition.clone()
    }
}

fn transform_block(
    context: &mut Context,
    block: &Block,
    result_type: &Type,
    local_variables: &FnvHashMap<&str, Type>,
) -> Block {
    let (instructions, terminal_instruction) = transform_instructions(
        context,
        block.instructions(),
        block.terminal_instruction(),
        result_type,
        local_variables,
    );

    Block::new(instructions, terminal_instruction)
}

fn transform_instructions(
    context: &mut Context,
    instructions: &[Instruction],
    terminal_instruction: &TerminalInstruction,
    result_type: &Type,
    local_variables: &FnvHashMap<&str, Type>,
) -> (Vec<Instruction>, TerminalInstruction) {
    let mut rest_instructions = vec![];
    let mut terminal_instruction = terminal_instruction.clone();

    for instruction in instructions.iter().rev() {
        match instruction {
            Instruction::If(if_) if has_source_call(if_.then()) || has_source_call(if_.else_()) => {
                // Allow inlining a instruction.
                if rest_instructions.len() <= 1 {
                    rest_instructions = vec![If::new(
                        void_type(),
                        if_.condition().clone(),
                        transform_if_block_with_rest_instructions(
                            context,
                            if_.name(),
                            if_.then(),
                            result_type,
                            local_variables,
                            &rest_instructions,
                            &terminal_instruction,
                        ),
                        transform_if_block_with_rest_instructions(
                            context,
                            if_.name(),
                            if_.else_(),
                            result_type,
                            local_variables,
                            &rest_instructions,
                            &terminal_instruction,
                        ),
                        "",
                    )
                    .into()];
                } else {
                    rest_instructions.reverse();

                    let environment = get_continuation_environment(
                        &rest_instructions,
                        &terminal_instruction,
                        local_variables,
                    );
                    let continuation = create_continuation(
                        context,
                        &rest_instructions,
                        &terminal_instruction,
                        result_type,
                        &environment,
                    )
                    .into();

                    rest_instructions = vec![If::new(
                        void_type(),
                        if_.condition().clone(),
                        transform_if_block_with_continuation(
                            context,
                            if_.name(),
                            if_.then(),
                            result_type,
                            local_variables,
                            &continuation,
                            &environment,
                        ),
                        transform_if_block_with_continuation(
                            context,
                            if_.name(),
                            if_.else_(),
                            result_type,
                            local_variables,
                            &continuation,
                            &environment,
                        ),
                        "",
                    )
                    .into()];
                }

                terminal_instruction = TerminalInstruction::Unreachable;
            }
            _ => rest_instructions.push(instruction.clone()),
        }
    }

    rest_instructions.reverse();

    (rest_instructions, terminal_instruction)
}

fn has_source_call(block: &Block) -> bool {
    block
        .instructions()
        .iter()
        .any(|instruction| match instruction {
            Instruction::Call(call) => {
                call.type_().calling_convention() == types::CallingConvention::Source
            }
            Instruction::If(if_) => has_source_call(if_.then()) || has_source_call(if_.else_()),
            _ => false,
        })
}

fn transform_if_block_with_rest_instructions(
    context: &mut Context,
    if_name: &str,
    block: &Block,
    result_type: &Type,
    local_variables: &FnvHashMap<&str, Type>,
    rest_instructions: &[Instruction],
    terminal_instruction: &TerminalInstruction,
) -> Block {
    transform_block(
        context,
        &Block::new(
            block
                .instructions()
                .iter()
                .cloned()
                .chain(
                    if let TerminalInstruction::Branch(branch) = block.terminal_instruction() {
                        rest_instructions
                            .iter()
                            .map(|instruction| {
                                expression_conversion::convert_in_instruction(
                                    instruction,
                                    &|expression| {
                                        if expression == &Variable::new(if_name).into() {
                                            branch.expression().clone()
                                        } else {
                                            expression.clone()
                                        }
                                    },
                                )
                            })
                            .collect::<Vec<_>>()
                    } else {
                        vec![]
                    },
                )
                .collect(),
            match (block.terminal_instruction(), terminal_instruction) {
                // Outer blocks' terminal instructions should be converted into return or
                // unreachable already at this point.
                (_, TerminalInstruction::Branch(_)) => unreachable!(),
                (TerminalInstruction::Return(return_), _) => return_.clone().into(),
                (TerminalInstruction::Unreachable, _)
                | (TerminalInstruction::Branch(_), TerminalInstruction::Unreachable) => {
                    TerminalInstruction::Unreachable
                }
                (TerminalInstruction::Branch(branch), return_) => {
                    expression_conversion::convert_in_terminal_instruction(return_, &|expression| {
                        if expression == &Variable::new(if_name).into() {
                            branch.expression().clone()
                        } else {
                            expression.clone()
                        }
                    })
                }
            },
        ),
        result_type,
        local_variables,
    )
}

fn transform_if_block_with_continuation(
    context: &mut Context,
    if_name: &str,
    block: &Block,
    result_type: &Type,
    local_variables: &FnvHashMap<&str, Type>,
    continuation: &Expression,
    environment: &[(&str, Type)],
) -> Block {
    if let TerminalInstruction::Branch(branch) = block.terminal_instruction() {
        let result_name = context.name_generator.generate();

        transform_block(
            context,
            &Block::new(
                block
                    .instructions()
                    .iter()
                    .cloned()
                    .chain([Call::new(
                        types::Function::new(
                            environment.iter().map(|(_, type_)| type_.clone()).collect(),
                            result_type.clone(),
                            types::CallingConvention::Source,
                        ),
                        continuation.clone(),
                        environment
                            .iter()
                            .map(|(name, _)| {
                                if name == &if_name {
                                    branch.expression().clone()
                                } else {
                                    Variable::new(*name).into()
                                }
                            })
                            .collect(),
                        &result_name,
                    )
                    .into()])
                    .collect(),
                Return::new(result_type.clone(), Variable::new(result_name)),
            ),
            result_type,
            local_variables,
        )
    } else {
        transform_block(context, block, result_type, local_variables)
    }
}

fn create_continuation(
    context: &mut Context,
    instructions: &[Instruction],
    terminal_instruction: &TerminalInstruction,
    result_type: &Type,
    environment: &[(&str, Type)],
) -> Variable {
    let name = context.name_generator.generate();
    let block = transform_block(
        context,
        &Block::new(instructions.to_vec(), terminal_instruction.clone()),
        result_type,
        &environment.iter().cloned().collect(),
    );

    context.function_definitions.push(FunctionDefinition::new(
        &name,
        environment
            .iter()
            .map(|(name, type_)| Argument::new(*name, type_.clone()))
            .collect(),
        result_type.clone(),
        block,
        FunctionDefinitionOptions::new()
            .set_address_named(false)
            .set_calling_convention(types::CallingConvention::Source)
            .set_linkage(Linkage::Internal),
    ));

    Variable::new(name)
}

fn get_continuation_environment<'a>(
    instructions: &'a [Instruction],
    terminal_instruction: &'a TerminalInstruction,
    local_variables: &FnvHashMap<&str, Type>,
) -> Vec<(&'a str, Type)> {
    free_variable::collect(instructions, terminal_instruction)
        .iter()
        .flat_map(|&name| local_variables.get(name).map(|type_| (name, type_.clone())))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::{name, type_check},
        types::void_type,
    };
    use pretty_assertions::assert_eq;

    fn flatten_module(module: &Module) -> Module {
        let flattened = flatten(module);

        name::check(&flattened).unwrap();
        type_check::check(&flattened).unwrap();

        // Test reproducibility.
        assert_eq!(flattened, flatten(module));

        flattened
    }

    fn create_function_type(arguments: Vec<Type>, result: impl Into<Type>) -> types::Function {
        types::Function::new(arguments, result, types::CallingConvention::Source)
    }

    fn create_function_definition(
        name: impl Into<String>,
        arguments: Vec<Argument>,
        result_type: impl Into<Type>,
        body: Block,
    ) -> FunctionDefinition {
        FunctionDefinition::new(name, arguments, result_type, body, Default::default())
    }

    #[test]
    fn transform_empty_module() {
        flatten_module(&Module::new(vec![], vec![], vec![], vec![]));
    }

    #[test]
    fn transform_function_definition() {
        flatten_module(&Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![],
                types::Primitive::Float64,
                Block::new(
                    vec![],
                    Return::new(types::Primitive::Float64, Primitive::Float64(42.0)),
                ),
            )],
        ));
    }

    #[test]
    fn flatten_if_with_return() {
        let function_type = create_function_type(
            vec![types::Primitive::Float64.into()],
            types::Primitive::Float64,
        );

        flatten_module(&Module::new(
            vec![],
            vec![FunctionDeclaration::new("f", function_type.clone())],
            vec![],
            vec![create_function_definition(
                "g",
                vec![],
                types::Primitive::Float64,
                Block::new(
                    vec![If::new(
                        void_type(),
                        Primitive::Boolean(true),
                        Block::new(
                            vec![Call::new(
                                function_type,
                                Variable::new("f"),
                                vec![Primitive::Float64(42.0).into()],
                                "x",
                            )
                            .into()],
                            Return::new(types::Primitive::Float64, Variable::new("x")),
                        ),
                        Block::new(vec![], TerminalInstruction::Unreachable),
                        "_",
                    )
                    .into()],
                    TerminalInstruction::Unreachable,
                ),
            )],
        ));
    }

    #[test]
    fn flatten_if_with_branch() {
        let function_type = create_function_type(
            vec![types::Primitive::Float64.into()],
            types::Primitive::Float64,
        );

        flatten_module(&Module::new(
            vec![],
            vec![FunctionDeclaration::new("f", function_type.clone())],
            vec![],
            vec![create_function_definition(
                "g",
                vec![],
                types::Primitive::Float64,
                Block::new(
                    vec![If::new(
                        types::Primitive::Float64,
                        Primitive::Boolean(true),
                        Block::new(
                            vec![Call::new(
                                function_type,
                                Variable::new("f"),
                                vec![Primitive::Float64(42.0).into()],
                                "x",
                            )
                            .into()],
                            Branch::new(types::Primitive::Float64, Variable::new("x")),
                        ),
                        Block::new(vec![], TerminalInstruction::Unreachable),
                        "y",
                    )
                    .into()],
                    Return::new(types::Primitive::Float64, Variable::new("y")),
                ),
            )],
        ));
    }

    #[test]
    fn flatten_nested_if() {
        let function_type = create_function_type(
            vec![types::Primitive::Float64.into()],
            types::Primitive::Float64,
        );

        assert_eq!(
            flatten_module(&Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", function_type.clone())],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![If::new(
                            types::Primitive::Float64,
                            Primitive::Boolean(true),
                            Block::new(
                                vec![If::new(
                                    types::Primitive::Float64,
                                    Primitive::Boolean(true),
                                    Block::new(
                                        vec![Call::new(
                                            function_type.clone(),
                                            Variable::new("f"),
                                            vec![Primitive::Float64(42.0).into()],
                                            "x",
                                        )
                                        .into()],
                                        Branch::new(types::Primitive::Float64, Variable::new("x")),
                                    ),
                                    Block::new(vec![], TerminalInstruction::Unreachable),
                                    "y",
                                )
                                .into()],
                                Branch::new(types::Primitive::Float64, Variable::new("y")),
                            ),
                            Block::new(vec![], TerminalInstruction::Unreachable),
                            "z",
                        )
                        .into()],
                        Return::new(types::Primitive::Float64, Variable::new("z")),
                    ),
                )],
            )),
            Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", function_type.clone())],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![If::new(
                            void_type(),
                            Primitive::Boolean(true),
                            Block::new(
                                vec![If::new(
                                    void_type(),
                                    Primitive::Boolean(true),
                                    Block::new(
                                        vec![Call::new(
                                            function_type,
                                            Variable::new("f"),
                                            vec![Primitive::Float64(42.0).into()],
                                            "x",
                                        )
                                        .into()],
                                        Return::new(types::Primitive::Float64, Variable::new("x")),
                                    ),
                                    Block::new(vec![], TerminalInstruction::Unreachable),
                                    "",
                                )
                                .into()],
                                TerminalInstruction::Unreachable,
                            ),
                            Block::new(vec![], TerminalInstruction::Unreachable),
                            "",
                        )
                        .into()],
                        TerminalInstruction::Unreachable,
                    ),
                )]
            )
        );
    }

    #[test]
    fn flatten_nested_if_with_continuation_of_instruction() {
        let function_type = create_function_type(
            vec![types::Primitive::Float64.into()],
            types::Primitive::Float64,
        );

        assert_eq!(
            flatten_module(&Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", function_type.clone())],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![
                            If::new(
                                types::Primitive::Float64,
                                Primitive::Boolean(true),
                                Block::new(
                                    vec![If::new(
                                        types::Primitive::Float64,
                                        Primitive::Boolean(true),
                                        Block::new(
                                            vec![Call::new(
                                                function_type.clone(),
                                                Variable::new("f"),
                                                vec![Primitive::Float64(42.0).into()],
                                                "x",
                                            )
                                            .into()],
                                            Branch::new(
                                                types::Primitive::Float64,
                                                Variable::new("x")
                                            ),
                                        ),
                                        Block::new(vec![], TerminalInstruction::Unreachable),
                                        "y",
                                    )
                                    .into()],
                                    Branch::new(types::Primitive::Float64, Variable::new("y")),
                                ),
                                Block::new(vec![], TerminalInstruction::Unreachable),
                                "z",
                            )
                            .into(),
                            Store::new(
                                types::Primitive::Float64,
                                Variable::new("z"),
                                Undefined::new(types::Pointer::new(types::Primitive::Float64)),
                            )
                            .into()
                        ],
                        Return::new(types::Primitive::Float64, Variable::new("z")),
                    ),
                )],
            )),
            Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", function_type.clone())],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![],
                    types::Primitive::Float64,
                    Block::new(
                        vec![If::new(
                            void_type(),
                            Primitive::Boolean(true),
                            Block::new(
                                vec![If::new(
                                    void_type(),
                                    Primitive::Boolean(true),
                                    Block::new(
                                        vec![
                                            Call::new(
                                                function_type,
                                                Variable::new("f"),
                                                vec![Primitive::Float64(42.0).into()],
                                                "x",
                                            )
                                            .into(),
                                            Store::new(
                                                types::Primitive::Float64,
                                                Variable::new("x"),
                                                Undefined::new(types::Pointer::new(
                                                    types::Primitive::Float64
                                                )),
                                            )
                                            .into()
                                        ],
                                        Return::new(types::Primitive::Float64, Variable::new("x")),
                                    ),
                                    Block::new(vec![], TerminalInstruction::Unreachable),
                                    "",
                                )
                                .into()],
                                TerminalInstruction::Unreachable,
                            ),
                            Block::new(vec![], TerminalInstruction::Unreachable),
                            "",
                        )
                        .into()],
                        TerminalInstruction::Unreachable,
                    ),
                )]
            )
        );
    }

    #[test]
    fn flatten_if_with_multiple_free_variables() {
        let function_type = create_function_type(
            vec![types::Primitive::Float64.into()],
            types::Primitive::Float64,
        );

        flatten_module(&Module::new(
            vec![],
            vec![FunctionDeclaration::new("f", function_type.clone())],
            vec![],
            vec![create_function_definition(
                "g",
                vec![
                    Argument::new("a", types::Primitive::Float64),
                    Argument::new("b", types::Primitive::Float64),
                ],
                types::Primitive::Float64,
                Block::new(
                    vec![If::new(
                        types::Primitive::Float64,
                        Primitive::Boolean(true),
                        Block::new(
                            vec![Call::new(
                                function_type,
                                Variable::new("f"),
                                vec![Primitive::Float64(42.0).into()],
                                "x",
                            )
                            .into()],
                            Branch::new(types::Primitive::Float64, Variable::new("x")),
                        ),
                        Block::new(vec![], TerminalInstruction::Unreachable),
                        "y",
                    )
                    .into()],
                    Return::new(
                        types::Primitive::Float64,
                        ArithmeticOperation::new(
                            types::Primitive::Float64,
                            ArithmeticOperator::Add,
                            ArithmeticOperation::new(
                                types::Primitive::Float64,
                                ArithmeticOperator::Add,
                                Variable::new("a"),
                                Variable::new("b"),
                            ),
                            Variable::new("y"),
                        ),
                    ),
                ),
            )],
        ));
    }

    #[test]
    fn preserve_if() {
        let module = Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![If::new(
                        types::Primitive::PointerInteger,
                        Primitive::Boolean(true),
                        Block::new(
                            vec![],
                            Branch::new(
                                types::Primitive::PointerInteger,
                                Primitive::PointerInteger(1),
                            ),
                        ),
                        Block::new(
                            vec![],
                            Branch::new(
                                types::Primitive::PointerInteger,
                                Primitive::PointerInteger(2),
                            ),
                        ),
                        "y",
                    )
                    .into()],
                    Return::new(types::Primitive::PointerInteger, Variable::new("y")),
                ),
            )],
        );

        assert_eq!(flatten_module(&module), module);
    }

    #[test]
    fn preserve_if_with_non_call_instruction() {
        let module = Module::new(
            vec![],
            vec![],
            vec![],
            vec![create_function_definition(
                "f",
                vec![],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![If::new(
                        types::Primitive::PointerInteger,
                        Primitive::Boolean(true),
                        Block::new(
                            vec![Load::new(
                                types::Primitive::PointerInteger,
                                Undefined::new(types::Pointer::new(
                                    types::Primitive::PointerInteger,
                                )),
                                "x",
                            )
                            .into()],
                            Branch::new(
                                types::Primitive::PointerInteger,
                                Primitive::PointerInteger(1),
                            ),
                        ),
                        Block::new(
                            vec![],
                            Branch::new(
                                types::Primitive::PointerInteger,
                                Primitive::PointerInteger(2),
                            ),
                        ),
                        "y",
                    )
                    .into()],
                    Return::new(types::Primitive::PointerInteger, Variable::new("y")),
                ),
            )],
        );

        assert_eq!(flatten_module(&module), module);
    }

    #[test]
    fn preserve_if_with_target_call() {
        let function_type = types::Function::new(
            vec![],
            types::Primitive::PointerInteger,
            types::CallingConvention::Target,
        );

        let module = Module::new(
            vec![],
            vec![FunctionDeclaration::new("f", function_type.clone())],
            vec![],
            vec![create_function_definition(
                "g",
                vec![],
                types::Primitive::PointerInteger,
                Block::new(
                    vec![If::new(
                        types::Primitive::PointerInteger,
                        Primitive::Boolean(true),
                        Block::new(
                            vec![Call::new(function_type, Variable::new("f"), vec![], "x").into()],
                            Branch::new(
                                types::Primitive::PointerInteger,
                                Primitive::PointerInteger(1),
                            ),
                        ),
                        Block::new(
                            vec![],
                            Branch::new(
                                types::Primitive::PointerInteger,
                                Primitive::PointerInteger(2),
                            ),
                        ),
                        "y",
                    )
                    .into()],
                    Return::new(types::Primitive::PointerInteger, Variable::new("y")),
                ),
            )],
        );

        assert_eq!(flatten_module(&module), module);
    }

    #[test]
    fn transform_if_with_source_call() {
        let function_type = types::Function::new(
            vec![],
            types::Primitive::PointerInteger,
            types::CallingConvention::Source,
        );

        assert_eq!(
            flatten_module(&Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", function_type.clone())],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![If::new(
                            types::Primitive::PointerInteger,
                            Primitive::Boolean(true),
                            Block::new(
                                vec![Call::new(
                                    function_type.clone(),
                                    Variable::new("f"),
                                    vec![],
                                    "x"
                                )
                                .into()],
                                Branch::new(
                                    types::Primitive::PointerInteger,
                                    Primitive::PointerInteger(1),
                                ),
                            ),
                            Block::new(
                                vec![],
                                Branch::new(
                                    types::Primitive::PointerInteger,
                                    Primitive::PointerInteger(2),
                                ),
                            ),
                            "y",
                        )
                        .into()],
                        Return::new(types::Primitive::PointerInteger, Variable::new("y")),
                    ),
                )],
            )),
            Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", function_type.clone())],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![If::new(
                            void_type(),
                            Primitive::Boolean(true),
                            Block::new(
                                vec![Call::new(function_type, Variable::new("f"), vec![], "x")
                                    .into()],
                                Return::new(
                                    types::Primitive::PointerInteger,
                                    Primitive::PointerInteger(1),
                                ),
                            ),
                            Block::new(
                                vec![],
                                Return::new(
                                    types::Primitive::PointerInteger,
                                    Primitive::PointerInteger(2),
                                ),
                            ),
                            "",
                        )
                        .into()],
                        TerminalInstruction::Unreachable
                    ),
                )],
            )
        );
    }

    #[test]
    fn preserve_call_after_preserved_if() {
        let function_type = create_function_type(
            vec![types::Primitive::Float64.into()],
            types::Primitive::Float64,
        );
        let module = Module::new(
            vec![],
            vec![FunctionDeclaration::new("f", function_type.clone())],
            vec![],
            vec![create_function_definition(
                "g",
                vec![],
                types::Primitive::Float64,
                Block::new(
                    vec![
                        If::new(
                            types::Primitive::Float64,
                            Primitive::Boolean(true),
                            Block::new(
                                vec![Load::new(
                                    types::Primitive::Float64,
                                    Undefined::new(types::Pointer::new(types::Primitive::Float64)),
                                    "x",
                                )
                                .into()],
                                Branch::new(types::Primitive::Float64, Variable::new("x")),
                            ),
                            Block::new(vec![], TerminalInstruction::Unreachable),
                            "y",
                        )
                        .into(),
                        Call::new(
                            function_type,
                            Variable::new("f"),
                            vec![Variable::new("y").into()],
                            "z",
                        )
                        .into(),
                    ],
                    Return::new(types::Primitive::Float64, Variable::new("z")),
                ),
            )],
        );

        assert_eq!(flatten_module(&module), module);
    }
}
