use super::error::ReferenceCountError;
use super::expression_cloner::ExpressionCloner;
use crate::build::{InstructionBuilder, TypedExpression};
use crate::ir::*;
use crate::types::Type;
use std::{collections::HashSet, rc::Rc};

pub struct ExpressionMover {
    expression_cloner: Rc<ExpressionCloner>,
}

impl ExpressionMover {
    pub fn new(expression_cloner: Rc<ExpressionCloner>) -> Self {
        Self { expression_cloner }
    }

    pub fn move_expression(
        &self,
        builder: &InstructionBuilder,
        expression: &Expression,
        type_: &Type,
        owned_variables: &HashSet<String>,
        moved_variables: &HashSet<String>,
    ) -> Result<HashSet<String>, ReferenceCountError> {
        let move_expression =
            |expression: &Expression, type_: &Type, moved_variables: &HashSet<String>| {
                self.move_expression(builder, expression, type_, owned_variables, moved_variables)
            };

        Ok(match expression {
            Expression::BitCast(bit_cast) => {
                move_expression(bit_cast.expression(), bit_cast.from(), moved_variables)?
            }
            Expression::BitwiseOperation(operation) => {
                let moved_variables =
                    move_expression(operation.rhs(), &operation.type_().into(), moved_variables)?;

                move_expression(operation.lhs(), &operation.type_().into(), &moved_variables)?
            }
            Expression::Record(record) => {
                let mut moved_variables = moved_variables.clone();

                for (expression, type_) in record
                    .elements()
                    .iter()
                    .zip(record.type_().elements())
                    .rev()
                {
                    moved_variables = move_expression(expression, type_, &moved_variables)?;
                }

                moved_variables
            }
            Expression::Union(_) => return Err(ReferenceCountError::UnionNotSupported),
            Expression::Variable(variable) => {
                if owned_variables.contains(variable.name())
                    && moved_variables.contains(variable.name())
                {
                    self.expression_cloner.clone_expression(
                        builder,
                        &TypedExpression::new(expression.clone(), type_.clone()),
                    )?;

                    moved_variables
                        .iter()
                        .cloned()
                        .chain(vec![variable.name().into()])
                        .collect()
                } else {
                    moved_variables.clone()
                }
            }
            Expression::AlignOf(_)
            | Expression::Primitive(_)
            | Expression::SizeOf(_)
            | Expression::Undefined(_) => moved_variables.clone(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{build::NameGenerator, types};
    use pretty_assertions::assert_eq;

    fn initialize_expression_mover() -> (ExpressionMover, InstructionBuilder) {
        let name_generator = Rc::new(NameGenerator::new("test_").into());

        (
            ExpressionMover::new(ExpressionCloner::new().into()),
            InstructionBuilder::new(name_generator),
        )
    }

    #[test]
    fn convert_align_of() {
        let (mover, builder) = initialize_expression_mover();
        let moved_variables = mover
            .move_expression(
                &builder,
                &AlignOf::new(types::Primitive::PointerInteger).into(),
                &AlignOf::RESULT_TYPE.into(),
                &Default::default(),
                &Default::default(),
            )
            .unwrap();

        assert!(builder.into_instructions().is_empty());
        assert_eq!(moved_variables, Default::default());
    }

    #[test]
    fn convert_moved_variable() {
        let (mover, builder) = initialize_expression_mover();
        let moved_variables = mover
            .move_expression(
                &builder,
                &Variable::new("x").into(),
                &types::Pointer::new(types::Primitive::Float64).into(),
                &vec!["x".into()].into_iter().collect(),
                &vec!["x".into()].into_iter().collect(),
            )
            .unwrap();

        assert!(!builder.into_instructions().is_empty());
        assert_eq!(moved_variables, vec!["x".into()].into_iter().collect());
    }

    #[test]
    fn convert_moved_variable_in_record() {
        let pointer_type = types::Pointer::new(types::Primitive::Float64);
        let variable = Variable::new("x");

        let (mover, builder) = initialize_expression_mover();
        let moved_variables = mover
            .move_expression(
                &builder,
                &Record::new(
                    types::Record::new(vec![
                        pointer_type.clone().into(),
                        pointer_type.clone().into(),
                    ]),
                    vec![variable.clone().into(), variable.into()],
                )
                .into(),
                &pointer_type.into(),
                &vec!["x".into()].into_iter().collect(),
                &vec!["x".into()].into_iter().collect(),
            )
            .unwrap();

        assert!(!builder.into_instructions().is_empty());
        assert_eq!(moved_variables, vec!["x".into()].into_iter().collect());
    }
}
