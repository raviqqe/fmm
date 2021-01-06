use super::alternative::Alternative;
use super::default_alternative::DefaultAlternative;
use super::expression::Expression;
use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct Switch {
    condition_type: Type,
    result_type: Type,
    condition: Expression,
    alternatives: Vec<Alternative>,
    default_alternative: DefaultAlternative,
    name: String,
}

impl Switch {
    pub fn new(
        condition_type: impl Into<Type>,
        result_type: impl Into<Type>,
        condition: impl Into<Expression>,
        alternatives: Vec<Alternative>,
        default_alternative: DefaultAlternative,
        name: impl Into<String>,
    ) -> Self {
        Self {
            condition_type: condition_type.into(),
            result_type: result_type.into(),
            condition: condition.into(),
            alternatives,
            default_alternative,
            name: name.into(),
        }
    }

    pub fn condition_type(&self) -> &Type {
        &self.condition_type
    }

    pub fn result_type(&self) -> &Type {
        &self.result_type
    }

    pub fn condition(&self) -> &Expression {
        &self.condition
    }

    pub fn alternatives(&self) -> &[Alternative] {
        &self.alternatives
    }

    pub fn default_alternative(&self) -> &DefaultAlternative {
        &self.default_alternative
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}
