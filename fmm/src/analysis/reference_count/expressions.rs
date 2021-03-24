use super::reference_count::increment_count;
use crate::build;
use crate::ir::*;
use crate::types::Type;
use std::collections::{HashMap, HashSet};

pub fn convert_expression(
    expression: &Expression,
    variable_types: &HashMap<String, Type>,
    used_variables: &HashSet<String>,
) -> (Vec<Instruction>, Expression, HashSet<String>) {
    match expression {
        Expression::BitCast(bit_cast) => {
            let (instructions, expression, used_variables) =
                convert_expression(bit_cast.expression(), variable_types, &used_variables);

            // TODO Do bit casts move the values?
            (
                instructions,
                BitCast::new(bit_cast.from().clone(), bit_cast.to().clone(), expression).into(),
                used_variables,
            )
        }
        Expression::Record(record) => {
            let (instructions, elements, used_variables) = record.elements().iter().rev().fold(
                (vec![], vec![], HashSet::new()),
                |(all_instructions, all_elements, all_used_variables), element| {
                    let (instructions, expression, used_variables) =
                        convert_expression(element, variable_types, &used_variables);

                    (
                        all_instructions.into_iter().chain(instructions).collect(),
                        vec![element].into_iter().chain(all_elements).collect(),
                        all_used_variables.drain().chain(used_variables).collect(),
                    )
                },
            );

            (instructions, build::record(elements).into(), used_variables)
        }
        Expression::Union(union) => todo!(),
        Expression::Variable(variable) => (
            if used_variables.contains(variable.name()) {
                increment_count(variable, &variable_types[variable.name()])
            } else {
                vec![]
            },
            variable.clone().into(),
            vec![variable.name().into()].into_iter().collect(),
        ),
        Expression::AlignOf(_)
        | Expression::Primitive(_)
        | Expression::SizeOf(_)
        | Expression::Undefined(_) => (vec![], expression.clone(), used_variables.clone()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types;

    #[test]
    fn convert_align_of() {
        let align_of = AlignOf::new(types::Primitive::PointerInteger);

        pretty_assertions::assert_eq!(
            convert_expression(
                &align_of.clone().into(),
                &Default::default(),
                &Default::default()
            ),
            (vec![], align_of.clone().into(), Default::default())
        );
    }

    #[test]
    fn convert_used_variable() {
        let variable = Variable::new("x");

        let (instructions, expression, _) = convert_expression(
            &variable.clone().into(),
            &vec![(
                "x".into(),
                types::Pointer::new(types::Primitive::Float64).into(),
            )]
            .into_iter()
            .collect(),
            &vec!["x".into()].into_iter().collect(),
        );

        assert!(!instructions.is_empty());
        assert_eq!(expression, variable.into());
    }

    #[test]
    fn convert_used_variable_in_record() {
        let pointer_type = types::Pointer::new(types::Primitive::Float64);
        let variable = Variable::new("x");
        let record = Record::new(
            types::Record::new(vec![
                pointer_type.clone().into(),
                pointer_type.clone().into(),
            ]),
            vec![variable.clone().into(), variable.clone().into()],
        );

        let (instructions, expression, _) = convert_expression(
            &record.clone().into(),
            &vec![("x".into(), pointer_type.into())]
                .into_iter()
                .collect(),
            &vec!["x".into()].into_iter().collect(),
        );

        assert!(!instructions.is_empty());
        assert_eq!(expression, record.into());
    }
}
