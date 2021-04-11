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
        used_variables: &HashSet<String>,
    ) -> HashSet<String> {
        let move_expression =
            |expression: &Expression, type_: &Type, used_variables: &HashSet<String>| {
                self.move_expression(builder, expression, type_, owned_variables, used_variables)
            };

        match expression {
            Expression::BitCast(bit_cast) => {
                move_expression(bit_cast.expression(), bit_cast.from(), used_variables)
            }
            Expression::BitwiseOperation(operation) => {
                let used_variables =
                    move_expression(operation.rhs(), &operation.type_().into(), used_variables);

                move_expression(operation.lhs(), &operation.type_().into(), &used_variables)
            }
            Expression::Record(record) => {
                let mut used_variables = used_variables.clone();

                for (expression, type_) in record
                    .elements()
                    .iter()
                    .zip(record.type_().elements())
                    .rev()
                {
                    used_variables = move_expression(expression, type_, &used_variables);
                }

                used_variables
            }
            Expression::Union(_) => unimplemented!(),
            Expression::Variable(variable) => {
                if owned_variables.contains(variable.name())
                    && used_variables.contains(variable.name())
                {
                    self.expression_cloner.clone_expression(
                        builder,
                        &TypedExpression::new(expression.clone(), type_.clone()),
                    );

                    used_variables
                        .iter()
                        .cloned()
                        .chain(vec![variable.name().into()])
                        .collect()
                } else {
                    used_variables.clone()
                }
            }
            Expression::AlignOf(_)
            | Expression::Primitive(_)
            | Expression::SizeOf(_)
            | Expression::Undefined(_) => used_variables.clone(),
        }
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
            ExpressionMover::new(ExpressionCloner::new(name_generator).into()),
            InstructionBuilder::new(name_generator.clone()),
        )
    }

    #[test]
    fn convert_align_of() {
        let (mover, builder) = initialize_expression_mover();
        let used_variables = mover.move_expression(
            &builder,
            &AlignOf::new(types::Primitive::PointerInteger).into(),
            &AlignOf::RESULT_TYPE.into(),
            &Default::default(),
            &Default::default(),
        );

        assert!(builder.into_instructions().is_empty());
        assert_eq!(used_variables, Default::default());
    }

    #[test]
    fn convert_used_variable() {
        let (mover, builder) = initialize_expression_mover();
        let used_variables = mover.move_expression(
            &builder,
            &Variable::new("x").into(),
            &types::Pointer::new(types::Primitive::Float64).into(),
            &vec!["x".into()].into_iter().collect(),
            &vec!["x".into()].into_iter().collect(),
        );

        assert!(!builder.into_instructions().is_empty());
        assert_eq!(used_variables, vec!["x".into()].into_iter().collect());
    }

    #[test]
    fn convert_used_variable_in_record() {
        let pointer_type = types::Pointer::new(types::Primitive::Float64);
        let variable = Variable::new("x");

        let (mover, builder) = initialize_expression_mover();
        let used_variables = mover.move_expression(
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
        );

        assert!(!builder.into_instructions().is_empty());
        assert_eq!(used_variables, vec!["x".into()].into_iter().collect());
    }
}
