use super::{context::CpsContext, error::CpsError, stack::type_};
use crate::{
    analysis::cps::stack,
    build::{self, InstructionBuilder, TypedExpression},
    ir::*,
    types::{self, CallingConvention, Type},
};

struct Context<'a> {
    cps: &'a CpsContext,
    function_definitions: Vec<FunctionDefinition>,
}

pub fn transform(context: &CpsContext, module: &Module) -> Result<Module, CpsError> {
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
) -> Result<FunctionDefinition, CpsError> {
    Ok(
        if definition.type_().calling_convention() == CallingConvention::Target {
            FunctionDefinition::new(
                definition.name(),
                definition.arguments().to_vec(),
                definition.result_type().clone(),
                transform_block(context, definition.body())?,
                definition.options().clone(),
            )
        } else {
            definition.clone()
        },
    )
}

fn transform_block(context: &mut Context, block: &Block) -> Result<Block, CpsError> {
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
) -> Result<Vec<Instruction>, CpsError> {
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
) -> Result<Vec<Instruction>, CpsError> {
    let builder = InstructionBuilder::new(context.cps.name_generator());

    let result_pointer = builder.allocate_stack(call.type_().result().clone());

    // Store a zero value of a result type in case unexpected interruptions happen.
    // TODO Can we support blocking await in some way?
    builder.store(
        Undefined::new(call.type_().result().clone()),
        result_pointer.clone(),
    );

    let stack = stack::create(&builder)?;
    stack::push(&builder, stack.clone(), result_pointer.clone())?;

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
    builder.add_instruction(Load::new(
        result_pointer
            .type_()
            .to_pointer()
            .unwrap()
            .element()
            .clone(),
        result_pointer.expression().clone(),
        call.name(),
    ));

    stack::destroy(&builder, stack)?;

    Ok(builder.into_instructions())
}

fn compile_continuation(
    context: &mut Context,
    result_type: &Type,
) -> Result<TypedExpression, CpsError> {
    let name = context.cps.name_generator().borrow_mut().generate();

    context.function_definitions.push(FunctionDefinition::new(
        &name,
        vec![
            Argument::new("stack", type_()),
            Argument::new("result", result_type.clone()),
        ],
        context.cps.result_type().clone(),
        {
            let builder = InstructionBuilder::new(context.cps.name_generator());

            let result_pointer = stack::pop(
                &builder,
                build::variable("stack", type_()),
                types::Pointer::new(result_type.clone()),
            )?;
            builder.store(
                build::variable("result", result_type.clone()),
                result_pointer,
            );

            builder.return_(Undefined::new(context.cps.result_type().clone()))
        },
        FunctionDefinitionOptions::new()
            .set_address_named(false)
            .set_calling_convention(CallingConvention::Tail)
            .set_linkage(Linkage::Internal),
    ));

    Ok(build::variable(name, result_type.clone()))
}
