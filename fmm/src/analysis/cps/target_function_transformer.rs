use super::{context::CpsContext, error::CpsTransformationError, stack::STACK_TYPE};
use crate::{
    analysis::cps::stack,
    build::{self, InstructionBuilder, TypedExpression},
    ir::*,
    types::{self, CallingConvention, Type},
};

struct Context<'a> {
    pub cps: &'a CpsContext,
    pub function_definitions: Vec<FunctionDefinition>,
}

pub fn transform(context: &CpsContext, module: &Module) -> Result<Module, CpsTransformationError> {
    let mut context = Context {
        cps: context,
        function_definitions: vec![],
    };

    Ok(Module::new(
        module.variable_declarations().to_vec(),
        module.function_declarations().to_vec(),
        module.variable_definitions().to_vec(),
        module
            .function_definitions()
            .iter()
            .map(|definition| transform_definition(&mut context, definition))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .chain(context.function_definitions)
            .collect(),
    ))
}

fn transform_definition(
    context: &mut Context,
    definition: &FunctionDefinition,
) -> Result<FunctionDefinition, CpsTransformationError> {
    Ok(
        if definition.type_().calling_convention() == CallingConvention::Target {
            FunctionDefinition::new(
                definition.name(),
                definition.arguments().to_vec(),
                transform_block(context, definition.body())?,
                definition.result_type().clone(),
                definition.calling_convention(),
                definition.linkage(),
            )
        } else {
            definition.clone()
        },
    )
}

fn transform_block(context: &mut Context, block: &Block) -> Result<Block, CpsTransformationError> {
    Ok(Block::new(
        block
            .instructions()
            .iter()
            .map(|instruction| transform_instruction(context, instruction))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect(),
        block.terminal_instruction().clone(),
    ))
}

fn transform_instruction(
    context: &mut Context,
    instruction: &Instruction,
) -> Result<Vec<Instruction>, CpsTransformationError> {
    Ok(match instruction {
        Instruction::Call(call) => {
            if call.type_().calling_convention() == CallingConvention::Source {
                transform_source_function_call(context, call)?
            } else {
                vec![call.clone().into()]
            }
        }
        Instruction::If(if_) => vec![If::new(
            if_.type_().clone(),
            if_.condition().clone(),
            transform_block(context, if_.then())?,
            transform_block(context, if_.else_())?,
            if_.name(),
        )
        .into()],
        _ => vec![instruction.clone()],
    })
}

fn transform_source_function_call(
    context: &mut Context,
    call: &Call,
) -> Result<Vec<Instruction>, CpsTransformationError> {
    let builder = InstructionBuilder::new(context.cps.name_generator());

    let result_pointer = builder.allocate_stack(call.type_().result().clone());

    // Store a zero value of a result type in case unexpected interruptions happen.
    // TODO Can we support blocking await in some way?
    builder.store(
        Undefined::new(call.type_().result().clone()),
        result_pointer.clone(),
    );

    let stack = stack::create_stack(&builder)?;
    stack::push_to_stack(&builder, stack.clone(), result_pointer.clone())?;

    builder.call(
        TypedExpression::new(call.function().clone(), call.type_().clone()),
        [
            stack.clone(),
            compile_continuation(context, call.type_().result())?,
        ]
        .into_iter()
        .chain(
            call.arguments()
                .iter()
                .zip(call.type_().arguments())
                .map(|(expression, type_)| TypedExpression::new(expression.clone(), type_.clone())),
        )
        .collect(),
    )?;
    // TODO Construct a load instruction manually.
    let result = builder.load(result_pointer)?;

    stack::destroy_stack(&builder, stack)?;

    Ok(builder
        .into_instructions()
        .into_iter()
        .chain([PassThrough::new(
            result.type_().clone(),
            result.expression().clone(),
            call.name(),
        )
        .into()])
        .collect())
}

fn compile_continuation(
    context: &mut Context,
    result_type: &Type,
) -> Result<TypedExpression, CpsTransformationError> {
    let name = context.cps.name_generator().borrow_mut().generate();

    context.function_definitions.push(FunctionDefinition::new(
        &name,
        vec![
            Argument::new("stack", STACK_TYPE.clone()),
            Argument::new("result", result_type.clone()),
        ],
        {
            let builder = InstructionBuilder::new(context.cps.name_generator());

            let result_pointer = stack::pop_from_stack(
                &builder,
                build::variable("stack", STACK_TYPE.clone()),
                types::Pointer::new(result_type.clone()),
            )?;
            builder.store(
                build::variable("result", result_type.clone()),
                result_pointer,
            );

            builder.return_(Undefined::new(context.cps.result_type().clone()))
        },
        context.cps.result_type().clone(),
        CallingConvention::Tail,
        Linkage::Internal,
    ));

    Ok(build::variable(name, result_type.clone()))
}
