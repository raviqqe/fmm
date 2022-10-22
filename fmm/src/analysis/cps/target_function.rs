use super::{context::CpsContext, error::CpsError, stack};
use crate::{
    build::{self, InstructionBuilder, TypedExpression},
    ir::*,
    types::{self, CallingConvention, Type},
};

struct Context<'a> {
    cps: &'a CpsContext,
    function_definitions: Vec<FunctionDefinition>,
}

pub fn transform(context: &CpsContext, module: &mut Module) -> Result<(), CpsError> {
    let mut context = Context {
        cps: context,
        function_definitions: vec![],
    };

    for definition in module.function_definitions_mut() {
        transform_definition(&mut context, definition)?;
    }

    module
        .function_definitions_mut()
        .extend(context.function_definitions);

    Ok(())
}

fn transform_definition(
    context: &mut Context,
    definition: &mut FunctionDefinition,
) -> Result<(), CpsError> {
    if definition.type_().calling_convention() == CallingConvention::Target {
        transform_block(context, definition.body_mut())?;
    }

    Ok(())
}

fn transform_block(context: &mut Context, block: &mut Block) -> Result<(), CpsError> {
    let mut instructions = Vec::with_capacity(block.instructions().len());

    for instruction in block.instructions_mut().drain(..) {
        transform_instruction(context, instruction, &mut instructions)?;
    }

    *block.instructions_mut() = instructions;

    Ok(())
}

fn transform_instruction(
    context: &mut Context,
    instruction: Instruction,
    instructions: &mut Vec<Instruction>,
) -> Result<(), CpsError> {
    match instruction {
        Instruction::Call(call)
            if call.type_().calling_convention() == CallingConvention::Source =>
        {
            transform_source_function_call(context, call, instructions)?;
        }
        Instruction::If(mut if_) => {
            transform_block(context, if_.then_mut())?;
            transform_block(context, if_.else_mut())?;

            instructions.push(if_.into());
        }
        _ => instructions.push(instruction),
    }

    Ok(())
}

fn transform_source_function_call(
    context: &mut Context,
    call: Call,
    instructions: &mut Vec<Instruction>,
) -> Result<(), CpsError> {
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

    instructions.extend(builder.into_instructions());

    Ok(())
}

fn compile_continuation(
    context: &mut Context,
    result_type: &Type,
) -> Result<TypedExpression, CpsError> {
    let name = context.cps.name_generator().borrow_mut().generate();

    context.function_definitions.push(FunctionDefinition::new(
        &name,
        vec![
            Argument::new("stack", stack::type_()),
            Argument::new("result", result_type.clone()),
        ],
        context.cps.result_type().clone(),
        {
            let builder = InstructionBuilder::new(context.cps.name_generator());

            let result_pointer = stack::pop(
                &builder,
                build::variable("stack", stack::type_()),
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
