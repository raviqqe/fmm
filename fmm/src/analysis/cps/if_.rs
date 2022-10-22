use super::free_variable;
use crate::{
    analysis::{expression_conversion, local_variable},
    build::NameGenerator,
    ir::*,
    types::{self, void_type, Type},
};
use fnv::FnvHashMap;
use std::mem::replace;

struct Context {
    function_definitions: Vec<FunctionDefinition>,
    name_generator: NameGenerator,
}

// TODO Consider integrating this logic deeply with CPS transformation to omit
// extra CPS stack manipulation.
pub fn flatten(module: &mut Module) {
    let mut context = Context {
        function_definitions: vec![],
        name_generator: NameGenerator::new("_if_"),
    };

    for definition in module.function_definitions_mut() {
        transform_function_definition(&mut context, definition);
    }

    module
        .function_definitions_mut()
        .extend(context.function_definitions);
}

fn transform_function_definition(context: &mut Context, definition: &mut FunctionDefinition) {
    if definition.type_().calling_convention() != types::CallingConvention::Source {
        return;
    }

    let result_type = definition.result_type().clone();
    let local_variables = local_variable::collect(definition)
        .into_iter()
        .map(|(name, type_)| (name.to_owned(), type_))
        .collect();

    transform_block(
        context,
        definition.body_mut(),
        &result_type,
        &local_variables,
    );
}

fn transform_block(
    context: &mut Context,
    block: &mut Block,
    result_type: &Type,
    local_variables: &FnvHashMap<String, Type>,
) {
    let mut rest_instructions = vec![];
    let mut terminal_instruction = replace(
        block.terminal_instruction_mut(),
        TerminalInstruction::Unreachable,
    );

    for instruction in block.instructions_mut().drain(..).rev() {
        match instruction {
            Instruction::If(mut if_)
                if has_source_call(if_.then()) || has_source_call(if_.else_()) =>
            {
                rest_instructions.reverse();

                let name = if_.name().to_owned();

                if rest_instructions.is_empty() {
                    transform_if_block_with_rest_instructions(
                        context,
                        &name,
                        if_.then_mut(),
                        vec![],
                        terminal_instruction.clone(),
                        result_type,
                        local_variables,
                    );
                    transform_if_block_with_rest_instructions(
                        context,
                        &name,
                        if_.else_mut(),
                        vec![],
                        terminal_instruction,
                        result_type,
                        local_variables,
                    );
                } else if if_.then().terminal_instruction().is_branch()
                    && !if_.else_().terminal_instruction().is_branch()
                {
                    transform_if_block_with_rest_instructions(
                        context,
                        &name,
                        if_.then_mut(),
                        rest_instructions,
                        terminal_instruction.clone(),
                        result_type,
                        local_variables,
                    );
                    transform_if_block_with_rest_instructions(
                        context,
                        &name,
                        if_.else_mut(),
                        vec![],
                        terminal_instruction,
                        result_type,
                        local_variables,
                    );
                } else if !if_.then().terminal_instruction().is_branch()
                    && if_.else_().terminal_instruction().is_branch()
                {
                    transform_if_block_with_rest_instructions(
                        context,
                        &name,
                        if_.then_mut(),
                        vec![],
                        terminal_instruction.clone(),
                        result_type,
                        local_variables,
                    );
                    transform_if_block_with_rest_instructions(
                        context,
                        &name,
                        if_.else_mut(),
                        rest_instructions,
                        terminal_instruction,
                        result_type,
                        local_variables,
                    );
                } else {
                    let environment = get_continuation_environment(
                        &rest_instructions,
                        &terminal_instruction,
                        local_variables,
                    );
                    let continuation = create_continuation(
                        context,
                        Block::new(rest_instructions, terminal_instruction),
                        result_type,
                        &environment,
                    )
                    .into();

                    transform_if_block_with_continuation(
                        context,
                        &name,
                        if_.then_mut(),
                        result_type,
                        local_variables,
                        &continuation,
                        &environment,
                    );
                    transform_if_block_with_continuation(
                        context,
                        &name,
                        if_.else_mut(),
                        result_type,
                        local_variables,
                        &continuation,
                        &environment,
                    );
                }

                *if_.type_mut() = void_type().into();
                *if_.name_mut() = "".into();

                rest_instructions = vec![if_.into()];
                terminal_instruction = TerminalInstruction::Unreachable;
            }
            _ => rest_instructions.push(instruction.clone()),
        }
    }

    rest_instructions.reverse();

    *block.instructions_mut() = rest_instructions;
    *block.terminal_instruction_mut() = terminal_instruction;
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
    block: &mut Block,
    mut rest_instructions: Vec<Instruction>,
    terminal_instruction: TerminalInstruction,
    result_type: &Type,
    local_variables: &FnvHashMap<String, Type>,
) {
    match (block.terminal_instruction_mut(), terminal_instruction) {
        // Outer blocks' terminal instructions should be converted into return or
        // unreachable already at this point.
        (_, TerminalInstruction::Branch(_)) => unreachable!(),
        (TerminalInstruction::Return(_), _) | (TerminalInstruction::Unreachable, _) => {}
        (TerminalInstruction::Branch(_), TerminalInstruction::Unreachable) => {
            *block.terminal_instruction_mut() = TerminalInstruction::Unreachable;
        }
        (TerminalInstruction::Branch(branch), return_) => {
            let convert = |expression: &Expression| {
                if expression == &Variable::new(if_name).into() {
                    branch.expression().clone()
                } else {
                    expression.clone()
                }
            };

            // TODO Make expression conversion in place.
            for instruction in &mut rest_instructions {
                *instruction = expression_conversion::convert_in_instruction(instruction, &convert);
            }

            // TODO Make expression conversion in place.
            *block.terminal_instruction_mut() =
                expression_conversion::convert_in_terminal_instruction(&return_, &convert);
        }
    }

    block.instructions_mut().extend(rest_instructions);

    transform_block(context, block, result_type, local_variables);
}

fn transform_if_block_with_continuation(
    context: &mut Context,
    if_name: &str,
    block: &mut Block,
    result_type: &Type,
    local_variables: &FnvHashMap<String, Type>,
    continuation: &Expression,
    environment: &[(String, Type)],
) {
    if let TerminalInstruction::Branch(branch) = block.terminal_instruction() {
        let result_name = context.name_generator.generate();
        let call = Call::new(
            types::Function::new(
                environment.iter().map(|(_, type_)| type_.clone()).collect(),
                result_type.clone(),
                types::CallingConvention::Source,
            ),
            continuation.clone(),
            environment
                .iter()
                .map(|(name, _)| {
                    if name == if_name {
                        branch.expression().clone()
                    } else {
                        Variable::new(name).into()
                    }
                })
                .collect(),
            &result_name,
        );

        block.instructions_mut().push(call.into());
        *block.terminal_instruction_mut() =
            Return::new(result_type.clone(), Variable::new(result_name)).into();
    }

    transform_block(context, block, result_type, local_variables);
}

fn create_continuation(
    context: &mut Context,
    block: Block,
    result_type: &Type,
    environment: &[(String, Type)],
) -> Variable {
    let name = context.name_generator.generate();

    context.function_definitions.push(FunctionDefinition::new(
        &name,
        environment
            .iter()
            .map(|(name, type_)| Argument::new(name, type_.clone()))
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

fn get_continuation_environment(
    instructions: &[Instruction],
    terminal_instruction: &TerminalInstruction,
    local_variables: &FnvHashMap<String, Type>,
) -> Vec<(String, Type)> {
    free_variable::collect(instructions, terminal_instruction)
        .iter()
        .flat_map(|&name| {
            local_variables
                .get(name)
                .map(|type_| (name.into(), type_.clone()))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{analysis::validation, types::void_type};
    use pretty_assertions::assert_eq;

    fn flatten_module(mut module: Module) -> Module {
        validation::validate(&module).unwrap();

        let mut other = module.clone();

        flatten(&mut module);
        flatten(&mut other);

        validation::validate(&module).unwrap();

        // Test reproducibility.
        assert_eq!(module, other);

        module
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
        flatten_module(Module::new(vec![], vec![], vec![], vec![]));
    }

    #[test]
    fn transform_function_definition() {
        flatten_module(Module::new(
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

        flatten_module(Module::new(
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

        flatten_module(Module::new(
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
            flatten_module(Module::new(
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
            flatten_module(Module::new(
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
    fn flatten_if_with_continuation_of_if() {
        let function_type = create_function_type(
            vec![types::Primitive::Float64.into()],
            types::Primitive::Float64,
        );

        flatten_module(Module::new(
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
                        .into(),
                        If::new(
                            void_type(),
                            Primitive::Boolean(true),
                            Block::new(
                                vec![AllocateStack::new(types::Primitive::Float64, "z").into()],
                                Branch::new(void_type(), void_value()),
                            ),
                            Block::new(vec![], TerminalInstruction::Unreachable),
                            "",
                        )
                        .into(),
                    ],
                    Return::new(types::Primitive::Float64, Variable::new("y")),
                ),
            )],
        ));
    }

    #[test]
    fn flatten_if_with_duplicated_continuation_of_instruction() {
        let pointer_type = types::Pointer::new(types::Primitive::Float64);
        let function_type =
            create_function_type(vec![types::Primitive::Float64.into()], pointer_type.clone());

        assert_eq!(
            flatten_module(Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", function_type.clone())],
                vec![],
                vec![create_function_definition(
                    "g",
                    vec![],
                    pointer_type.clone(),
                    Block::new(
                        vec![
                            If::new(
                                pointer_type.clone(),
                                Primitive::Boolean(true),
                                Block::new(
                                    vec![Call::new(
                                        function_type.clone(),
                                        Variable::new("f"),
                                        vec![Primitive::Float64(42.0).into()],
                                        "x",
                                    )
                                    .into()],
                                    Branch::new(pointer_type.clone(), Variable::new("x")),
                                ),
                                Block::new(
                                    vec![],
                                    Branch::new(
                                        pointer_type.clone(),
                                        Undefined::new(pointer_type.clone())
                                    ),
                                ),
                                "y",
                            )
                            .into(),
                            AllocateStack::new(types::Primitive::Float64, "p").into()
                        ],
                        Return::new(pointer_type.clone(), Variable::new("p")),
                    ),
                )],
            )),
            Module::new(
                vec![],
                vec![FunctionDeclaration::new("f", function_type.clone())],
                vec![],
                vec![
                    create_function_definition(
                        "g",
                        vec![],
                        pointer_type.clone(),
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
                                        Call::new(
                                            types::Function::new(
                                                vec![],
                                                pointer_type.clone(),
                                                types::CallingConvention::Source
                                            ),
                                            Variable::new("_if_0"),
                                            vec![],
                                            "_if_1",
                                        )
                                        .into(),
                                    ],
                                    Return::new(pointer_type.clone(), Variable::new("_if_1")),
                                ),
                                Block::new(
                                    vec![Call::new(
                                        types::Function::new(
                                            vec![],
                                            pointer_type.clone(),
                                            types::CallingConvention::Source
                                        ),
                                        Variable::new("_if_0"),
                                        vec![],
                                        "_if_2",
                                    )
                                    .into()],
                                    Return::new(pointer_type.clone(), Variable::new("_if_2")),
                                ),
                                "",
                            )
                            .into()],
                            TerminalInstruction::Unreachable,
                        ),
                    ),
                    FunctionDefinition::new(
                        "_if_0",
                        vec![],
                        pointer_type.clone(),
                        Block::new(
                            vec![AllocateStack::new(types::Primitive::Float64, "p").into()],
                            Return::new(pointer_type, Variable::new("p"),),
                        ),
                        FunctionDefinitionOptions::new()
                            .set_address_named(false)
                            .set_linkage(Linkage::Internal)
                    )
                ]
            )
        );
    }

    #[test]
    fn flatten_nested_if_with_duplicated_continuation_of_instruction() {
        let pointer_type = types::Pointer::new(types::Primitive::Float64);
        let function_type =
            create_function_type(vec![types::Primitive::Float64.into()], pointer_type.clone());

        flatten_module(Module::new(
            vec![],
            vec![FunctionDeclaration::new("f", function_type.clone())],
            vec![],
            vec![create_function_definition(
                "g",
                vec![],
                pointer_type.clone(),
                Block::new(
                    vec![
                        If::new(
                            pointer_type.clone(),
                            Primitive::Boolean(true),
                            Block::new(
                                vec![If::new(
                                    pointer_type.clone(),
                                    Primitive::Boolean(true),
                                    Block::new(
                                        vec![Call::new(
                                            function_type,
                                            Variable::new("f"),
                                            vec![Primitive::Float64(42.0).into()],
                                            "x",
                                        )
                                        .into()],
                                        Branch::new(pointer_type.clone(), Variable::new("x")),
                                    ),
                                    Block::new(
                                        vec![],
                                        Branch::new(
                                            pointer_type.clone(),
                                            Undefined::new(pointer_type.clone()),
                                        ),
                                    ),
                                    "y",
                                )
                                .into()],
                                Branch::new(pointer_type.clone(), Variable::new("y")),
                            ),
                            Block::new(
                                vec![],
                                Branch::new(
                                    pointer_type.clone(),
                                    Undefined::new(pointer_type.clone()),
                                ),
                            ),
                            "z",
                        )
                        .into(),
                        AllocateStack::new(types::Primitive::Float64, "p").into(),
                    ],
                    Return::new(pointer_type, Variable::new("p")),
                ),
            )],
        ));
    }

    #[test]
    fn flatten_if_with_multiple_free_variables() {
        let function_type = create_function_type(
            vec![types::Primitive::Float64.into()],
            types::Primitive::Float64,
        );

        flatten_module(Module::new(
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

        assert_eq!(flatten_module(module.clone()), module);
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

        assert_eq!(flatten_module(module.clone()), module);
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

        assert_eq!(flatten_module(module.clone()), module);
    }

    #[test]
    fn transform_if_with_source_call() {
        let function_type = types::Function::new(
            vec![],
            types::Primitive::PointerInteger,
            types::CallingConvention::Source,
        );

        assert_eq!(
            flatten_module(Module::new(
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

        assert_eq!(flatten_module(module.clone()), module);
    }
}
