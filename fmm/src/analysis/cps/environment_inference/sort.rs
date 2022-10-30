use crate::{ir::*, types};
use indexmap::IndexMap;
use std::rc::Rc;

pub fn transform(module: &mut Module) {
    for definition in module.function_definitions_mut() {
        transform_function_definition(definition);
    }
}

fn transform_function_definition(definition: &mut FunctionDefinition) {
    if definition.type_().calling_convention() != types::CallingConvention::Source {
        return;
    }

    let mut variables = Default::default();

    collect_from_block(definition.body(), &mut variables);

    transform_block(definition.body_mut(), &variables);
}

fn transform_block(block: &mut Block, variables: &IndexMap<Rc<str>, usize>) {
    for instruction in block.instructions_mut().iter_mut().rev() {
        // Currently, we do not sort stack elements in if instructions as they do not
        // use stacks on memory.
        match instruction {
            Instruction::Call(call) => {
                if call.type_().calling_convention() == types::CallingConvention::Source {
                    call.environment_mut().sort_by(|one, other| {
                        variable_order(one, variables).cmp(&variable_order(other, variables))
                    });
                }
            }
            Instruction::If(if_) => {
                transform_block(if_.then_mut(), variables);
                transform_block(if_.else_mut(), variables);
            }
            _ => {}
        }
    }
}

fn collect_from_block(block: &Block, variables: &mut IndexMap<Rc<str>, usize>) {
    for instruction in block.instructions().iter().rev() {
        match instruction {
            Instruction::Call(call) => {
                for name in call.environment() {
                    variables.insert(name.clone(), variables.get(name).copied().unwrap_or(0) + 1);
                }
            }
            Instruction::If(if_) => {
                collect_from_block(if_.then(), variables);
                collect_from_block(if_.else_(), variables);
            }
            _ => {}
        }
    }
}

fn variable_order(name: &str, variables: &IndexMap<Rc<str>, usize>) -> (usize, usize) {
    (
        usize::MAX - variables[name],
        variables.get_index_of(name).unwrap(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analysis::validation;
    use pretty_assertions::assert_eq;

    fn transform_module(mut module: Module) -> Module {
        validation::validate(&module).unwrap();
        transform(&mut module);
        validation::validate(&module).unwrap();

        module
    }

    fn create_call(
        type_: types::Function,
        function: impl Into<Expression>,
        arguments: Vec<Expression>,
        environment: Vec<Rc<str>>,
        name: &str,
    ) -> Call {
        let mut call = Call::new(type_, function, arguments, name);

        *call.environment_mut() = environment;

        call
    }

    #[test]
    fn transform_no_free_variable() {
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
    fn transform_two_free_variables() {
        let function_type = types::Function::new(
            vec![],
            types::Primitive::PointerInteger,
            types::CallingConvention::Source,
        );

        assert_eq!(
            transform_module(Module::new(
                vec![],
                vec![FunctionDeclaration::new("g", function_type.clone())],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![
                            create_call(
                                function_type.clone(),
                                Variable::new("g"),
                                vec![],
                                vec!["y".into()],
                                "a"
                            )
                            .into(),
                            create_call(
                                function_type.clone(),
                                Variable::new("g"),
                                vec![],
                                vec!["x".into(), "y".into()],
                                "b"
                            )
                            .into()
                        ],
                        Return::new(types::Primitive::PointerInteger, Variable::new("b")),
                    ),
                    Default::default(),
                )],
            )),
            Module::new(
                vec![],
                vec![FunctionDeclaration::new("g", function_type.clone())],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![
                            create_call(
                                function_type.clone(),
                                Variable::new("g"),
                                vec![],
                                vec!["y".into()],
                                "a"
                            )
                            .into(),
                            create_call(
                                function_type,
                                Variable::new("g"),
                                vec![],
                                vec!["y".into(), "x".into(),],
                                "b"
                            )
                            .into()
                        ],
                        Return::new(types::Primitive::PointerInteger, Variable::new("b")),
                    ),
                    Default::default(),
                )],
            )
        );
    }

    // TODO Should we respect the last order instead?
    #[test]
    fn transform_two_free_variables_preferring_first_order() {
        let function_type = types::Function::new(
            vec![],
            types::Primitive::PointerInteger,
            types::CallingConvention::Source,
        );

        assert_eq!(
            transform_module(Module::new(
                vec![],
                vec![FunctionDeclaration::new("g", function_type.clone())],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![
                            create_call(
                                function_type.clone(),
                                Variable::new("g"),
                                vec![],
                                vec!["y".into(), "x".into()],
                                "a"
                            )
                            .into(),
                            create_call(
                                function_type.clone(),
                                Variable::new("g"),
                                vec![],
                                vec!["x".into(), "y".into()],
                                "b"
                            )
                            .into()
                        ],
                        Return::new(types::Primitive::PointerInteger, Variable::new("b")),
                    ),
                    Default::default(),
                )],
            )),
            Module::new(
                vec![],
                vec![FunctionDeclaration::new("g", function_type.clone())],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![
                            create_call(
                                function_type.clone(),
                                Variable::new("g"),
                                vec![],
                                vec!["y".into(), "x".into()],
                                "a"
                            )
                            .into(),
                            create_call(
                                function_type,
                                Variable::new("g"),
                                vec![],
                                vec!["y".into(), "x".into(),],
                                "b"
                            )
                            .into()
                        ],
                        Return::new(types::Primitive::PointerInteger, Variable::new("b")),
                    ),
                    Default::default(),
                )],
            )
        );
    }

    #[test]
    fn transform_two_free_variables_in_if() {
        let function_type = types::Function::new(
            vec![],
            types::Primitive::PointerInteger,
            types::CallingConvention::Source,
        );

        assert_eq!(
            transform_module(Module::new(
                vec![],
                vec![FunctionDeclaration::new("g", function_type.clone())],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![If::new(
                            types::Primitive::PointerInteger,
                            Primitive::Boolean(true),
                            Block::new(
                                vec![
                                    create_call(
                                        function_type.clone(),
                                        Variable::new("g"),
                                        vec![],
                                        vec!["y".into()],
                                        "a"
                                    )
                                    .into(),
                                    create_call(
                                        function_type.clone(),
                                        Variable::new("g"),
                                        vec![],
                                        vec!["x".into(), "y".into()],
                                        "b"
                                    )
                                    .into()
                                ],
                                Branch::new(types::Primitive::PointerInteger, Variable::new("b")),
                            ),
                            Block::new(
                                vec![
                                    create_call(
                                        function_type.clone(),
                                        Variable::new("g"),
                                        vec![],
                                        vec!["y".into()],
                                        "c"
                                    )
                                    .into(),
                                    create_call(
                                        function_type.clone(),
                                        Variable::new("g"),
                                        vec![],
                                        vec!["x".into(), "y".into()],
                                        "d"
                                    )
                                    .into()
                                ],
                                Branch::new(types::Primitive::PointerInteger, Variable::new("d")),
                            ),
                            "e",
                        )
                        .into()],
                        Return::new(types::Primitive::PointerInteger, Variable::new("e")),
                    ),
                    Default::default(),
                )],
            )),
            Module::new(
                vec![],
                vec![FunctionDeclaration::new("g", function_type.clone())],
                vec![],
                vec![FunctionDefinition::new(
                    "f",
                    vec![],
                    types::Primitive::PointerInteger,
                    Block::new(
                        vec![If::new(
                            types::Primitive::PointerInteger,
                            Primitive::Boolean(true),
                            Block::new(
                                vec![
                                    create_call(
                                        function_type.clone(),
                                        Variable::new("g"),
                                        vec![],
                                        vec!["y".into()],
                                        "a"
                                    )
                                    .into(),
                                    create_call(
                                        function_type.clone(),
                                        Variable::new("g"),
                                        vec![],
                                        vec!["y".into(), "x".into()],
                                        "b"
                                    )
                                    .into()
                                ],
                                Branch::new(types::Primitive::PointerInteger, Variable::new("b")),
                            ),
                            Block::new(
                                vec![
                                    create_call(
                                        function_type.clone(),
                                        Variable::new("g"),
                                        vec![],
                                        vec!["y".into()],
                                        "c"
                                    )
                                    .into(),
                                    create_call(
                                        function_type,
                                        Variable::new("g"),
                                        vec![],
                                        vec!["y".into(), "x".into()],
                                        "d"
                                    )
                                    .into()
                                ],
                                Branch::new(types::Primitive::PointerInteger, Variable::new("d")),
                            ),
                            "e",
                        )
                        .into()],
                        Return::new(types::Primitive::PointerInteger, Variable::new("e")),
                    ),
                    Default::default(),
                )],
            )
        );
    }
}
