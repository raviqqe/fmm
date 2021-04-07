use super::expression_lifetime_manager::ExpressionLifetimeManager;
use crate::ir::*;
use crate::types::Type;
use std::{collections::HashSet, rc::Rc};

pub struct ExpressionConverter {
    expression_lifetime_manager: Rc<ExpressionLifetimeManager>,
}

impl ExpressionConverter {
    pub fn new(expression_lifetime_manager: Rc<ExpressionLifetimeManager>) -> Self {
        Self {
            expression_lifetime_manager,
        }
    }

    pub fn count(
        &self,
        expression: &Expression,
        type_: &Type,
        used_variables: &HashSet<String>,
    ) -> (Vec<Instruction>, HashSet<String>) {
        match expression {
            Expression::BitCast(bit_cast) => {
                // TODO Do bit casts move the values?
                self.count(bit_cast.expression(), bit_cast.from(), &used_variables)
            }
            Expression::BitwiseOperation(operation) => {
                let (rhs_instructions, used_variables) =
                    self.count(operation.rhs(), &operation.type_().into(), &used_variables);
                let (lhs_instructions, used_variables) =
                    self.count(operation.lhs(), &operation.type_().into(), &used_variables);

                (
                    lhs_instructions
                        .into_iter()
                        .chain(rhs_instructions)
                        .collect(),
                    used_variables,
                )
            }
            Expression::Record(record) => {
                let (instructions, used_variables) = record
                    .elements()
                    .iter()
                    .zip(record.type_().elements())
                    .rev()
                    .fold(
                        (vec![], used_variables.clone()),
                        |(all_instructions, used_variables), (element, type_)| {
                            let (instructions, used_variables) =
                                self.count(element, type_, &used_variables);

                            (
                                instructions.into_iter().chain(all_instructions).collect(),
                                used_variables,
                            )
                        },
                    );

                (instructions, used_variables)
            }
            Expression::Union(_) => todo!(),
            Expression::Variable(variable) => (
                if used_variables.contains(variable.name()) {
                    self.expression_lifetime_manager
                        .clone_expression(expression, type_)
                } else {
                    vec![]
                },
                vec![variable.name().into()].into_iter().collect(),
            ),
            Expression::AlignOf(_)
            | Expression::Primitive(_)
            | Expression::SizeOf(_)
            | Expression::Undefined(_) => (vec![], used_variables.clone()),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use super::*;
    use crate::{build::NameGenerator, types};

    fn initialize_expression_converter() -> ExpressionConverter {
        ExpressionConverter::new(
            ExpressionLifetimeManager::new(RefCell::new(NameGenerator::new("rc")).into()).into(),
        )
    }

    #[test]
    fn convert_align_of() {
        pretty_assertions::assert_eq!(
            initialize_expression_converter().count(
                &AlignOf::new(types::Primitive::PointerInteger).into(),
                &AlignOf::RESULT_TYPE.into(),
                &Default::default()
            ),
            (vec![], Default::default())
        );
    }

    #[test]
    fn convert_used_variable() {
        let (instructions, used_variables) = initialize_expression_converter().count(
            &Variable::new("x").into(),
            &types::Pointer::new(types::Primitive::Float64).into(),
            &vec!["x".into()].into_iter().collect(),
        );

        assert!(!instructions.is_empty());
        assert_eq!(used_variables, vec!["x".into()].into_iter().collect());
    }

    #[test]
    fn convert_used_variable_in_record() {
        let pointer_type = types::Pointer::new(types::Primitive::Float64);
        let variable = Variable::new("x");

        let (instructions, used_variables) = initialize_expression_converter().count(
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
        );

        assert!(!instructions.is_empty());
        assert_eq!(used_variables, vec!["x".into()].into_iter().collect());
    }
}