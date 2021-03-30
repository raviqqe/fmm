use super::variable_lifetime_manager::VariableLifetimeManger;
use crate::ir::*;
use crate::types::Type;
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

pub struct ExpressionConverter {
    variable_lifetime_manager: Rc<VariableLifetimeManger>,
}

impl ExpressionConverter {
    pub fn new(variable_lifetime_manager: Rc<VariableLifetimeManger>) -> Self {
        Self {
            variable_lifetime_manager,
        }
    }

    pub fn convert(
        &self,
        expression: &Expression,
        variable_types: &HashMap<String, Type>,
        used_variables: &HashSet<String>,
    ) -> (Vec<Instruction>, HashSet<String>) {
        match expression {
            Expression::BitCast(bit_cast) => {
                // TODO Do bit casts move the values?
                let (instructions, used_variables) =
                    self.convert(bit_cast.expression(), variable_types, &used_variables);

                (instructions, used_variables)
            }
            Expression::Record(record) => {
                let (instructions, used_variables) = record.elements().iter().rev().fold(
                    (vec![], HashSet::new()),
                    |(all_instructions, all_used_variables), element| {
                        let (instructions, used_variables) =
                            self.convert(element, variable_types, &used_variables);

                        (
                            all_instructions.into_iter().chain(instructions).collect(),
                            all_used_variables
                                .into_iter()
                                .chain(used_variables)
                                .collect(),
                        )
                    },
                );

                (instructions, used_variables)
            }
            Expression::Union(_) => todo!(),
            Expression::Variable(variable) => (
                if used_variables.contains(variable.name()) {
                    self.variable_lifetime_manager
                        .clone_variable(variable, &variable_types[variable.name()])
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
            VariableLifetimeManger::new(RefCell::new(NameGenerator::new("rc")).into()).into(),
        )
    }

    #[test]
    fn convert_align_of() {
        pretty_assertions::assert_eq!(
            initialize_expression_converter().convert(
                &AlignOf::new(types::Primitive::PointerInteger).into(),
                &Default::default(),
                &Default::default()
            ),
            (vec![], Default::default())
        );
    }

    #[test]
    fn convert_used_variable() {
        let (instructions, used_variables) = initialize_expression_converter().convert(
            &Variable::new("x").into(),
            &vec![(
                "x".into(),
                types::Pointer::new(types::Primitive::Float64).into(),
            )]
            .into_iter()
            .collect(),
            &vec!["x".into()].into_iter().collect(),
        );

        assert!(!instructions.is_empty());
        assert_eq!(used_variables, vec!["x".into()].into_iter().collect());
    }

    #[test]
    fn convert_used_variable_in_record() {
        let pointer_type = types::Pointer::new(types::Primitive::Float64);
        let variable = Variable::new("x");

        let (instructions, used_variables) = initialize_expression_converter().convert(
            &Record::new(
                types::Record::new(vec![
                    pointer_type.clone().into(),
                    pointer_type.clone().into(),
                ]),
                vec![variable.clone().into(), variable.clone().into()],
            )
            .into(),
            &vec![("x".into(), pointer_type.into())]
                .into_iter()
                .collect(),
            &vec!["x".into()].into_iter().collect(),
        );

        assert!(!instructions.is_empty());
        assert_eq!(used_variables, vec!["x".into()].into_iter().collect());
    }
}
